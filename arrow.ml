
let sum = List.fold_left (+) 0
let (>>) f g x = g (f x)
let const x _ = x
let id x = x
(* let join = String.concat " " *)
(* let parenthesize = Printf.sprintf "(%s)" *)

let fprintf = Format.fprintf
let format_list list =
  Format.pp_print_list ~pp_sep:Format.pp_print_space list

module Color = struct
  let escape = Format.sprintf "\027[%dm"

  let reset = escape 0

  let red = escape 31
  let green = escape 32
  let yellow = escape 33

  module Format = struct
    let create code format_child f child =
      fprintf f "%s%a%s" (escape code) format_child child (escape 0)

    let red c = create 31 c
    let green c = create 32 c
    let yellow = create 33
  end
end

module Sexp = struct
  type t = Atom of string | List of t list

  let rec fold ~atom ~list = function
    | Atom a -> atom a
    | List l -> list (List.map (fold ~atom ~list) l)

  let size = fold ~atom:(const 1) ~list:sum

  let rec format f = function
    | Atom a -> fprintf f "%s" a
    | List l -> fprintf f "@[<hv 1>(%a)@]" (format_list format) l

  let to_string = Format.asprintf "%a" format
end

module Test = struct
  open Sexp

  let (=>) left right = print_char (if left = right then '.' else 'F')

  let a, b, c = Atom "a", Atom "b", Atom "c"

  let () = ()
    ; to_string (List [a; List [b; c]; List [a; List [b]]]) =>
        "(a (b c) (a (b)))"
end

let minimize ~cost (head :: tail) =
  let folder left right = if cost left < cost right then left else right in
  List.fold_left folder head tail

module Tree = struct
  type 'a t = One of 'a | Many of 'a t list

  let rec fold ~one ~many = function
    | One x -> one x
    | Many xs -> many (List.map (fold ~one ~many) xs)
end

module Change = struct
  type t =
    | Equality of Sexp.t
    | Addition of Sexp.t
    | Deletion of Sexp.t
    | Mutation of {deletion: Sexp.t; addition: Sexp.t}

  let fold ~equality ~addition ~deletion ~mutation = function
    | Equality sexp -> equality sexp
    | Addition sexp -> addition sexp
    | Deletion sexp -> deletion sexp
    | Mutation {deletion=d; addition=a} -> mutation (deletion d) (addition a)

  let cost =
    let size = Sexp.size in
    fold ~equality:size ~addition:size ~deletion:size ~mutation:(+)

  let rec format f = function
    | Equality sexp ->
        fprintf f "%a" Sexp.format sexp
    | Addition sexp ->
        fprintf f "%a" (Color.Format.green Sexp.format) sexp
    | Deletion sexp ->
        fprintf f "%a" (Color.Format.red Sexp.format) sexp
    | Mutation {deletion; addition} ->
        fprintf f "%a@ %a" format (Deletion deletion) format (Addition addition)

  let to_string = Format.asprintf "%a" format

  let rec debug_format f = function
    | Equality sexp ->
        fprintf f "%a" Sexp.format sexp
    | Addition sexp ->
        fprintf f "+%a" Sexp.format sexp
    | Deletion sexp ->
        fprintf f "-%a" Sexp.format sexp
    | Mutation {deletion; addition} ->
        fprintf f "%a@ %a" format (Deletion deletion) format (Addition addition)

  let debug = Format.asprintf "%a" debug_format
end

module Diff = struct
  type t = Change.t Tree.t

  let rec format f = function
    | Tree.One change ->
        fprintf f "%a" Change.format change
    | Tree.Many list ->
        fprintf f "@[<hv 1>(%a)@]" (format_list format) list

  let to_string = Format.asprintf "%a" format

  let cost = Tree.fold ~one:Change.cost ~many:sum

  let rec debug = let open Tree in function
    | One change -> Change.debug change
    | Many list -> "(" ^ String.concat " " (List.map debug list) ^ ")"
end

module Test = struct
  let (=>) left right = print_char (if left = right then '.' else 'F')
  let a, b, c = Sexp.(Atom "a", Atom "b", Atom "c")

  let () = let open Change in ()
    ; Diff.cost Tree.(Many [
        One (Equality a);
        One (Deletion b);
        Many [One (Addition c)];
      ]) => 3

  let () = let open Change in ()
    ; Diff.debug Tree.(Many [
        One (Equality a);
        Many [One (Equality b); One (Deletion c)];
      ]) => "(a (b -c))"
end

let rec diff: Sexp.t list -> Sexp.t list -> 'a Tree.t list = fun left right ->
  let open Tree in let open Change in
  let cost list = Diff.cost (Tree.Many list) in
  match left, right with
  | [], right -> List.map (fun sexp -> One (Addition sexp)) right
  | left, [] -> List.map (fun sexp -> One (Deletion sexp)) left
  | x :: xs, y :: ys when x = y ->
     (*
        The fact that when x = y we declare it a non-change without considering
        other options makes this diffing algorithm "impatient" (in patdiff sense).
        On one hand this makes our naive algorithm probably linear time in the
        common case of no diff (since we recurse only once), on the other hand
        this might lead to not the best looking diffs, e.g.:
          (a x x x a b b a) vs. (a b b a) => (a -x -x -x -a b b a)
      *)
     One (Equality x) :: diff xs ys
  | Sexp.List ls as x :: xs, (Sexp.List ms as y) :: ys ->
      minimize ~cost [
        One (Deletion x) :: diff xs right;
        One (Addition y) :: diff left ys;
        Many (diff ls ms) :: diff xs ys;
      ]
  | x :: xs, y :: ys ->
      minimize ~cost [
        One (Deletion x) :: diff xs right;
        One (Addition y) :: diff left ys;
        One (Deletion x) :: One (Addition y) :: diff xs ys;
      ]

module Test = struct
  let (=>) diffs string =
    Test.(=>) (String.concat " " (List.map Diff.debug diffs)) string
  let (!) _ = ()

  open Sexp

  let a, b, c = Atom "a", Atom "b", Atom "c"
  let x, y, z = Atom "x", Atom "y", Atom "z"

  let () = !"Test diff"
    ; diff [] [] => ""
    ; diff [] [a; b] => "+a +b"
    ; diff [a; b] [] => "-a -b"
    ; diff [a; b] [a] => "a -b"
    ; diff [a; b] [a; b] => "a b"
    ; diff [a; x; a; b; b; a] [a; b; b; a] => "a -x -a b b a"
    ; diff [List [a; List [b; c]]] [List [a; List [b]]] => "(a (b -c))"

    ; print_endline @@ Diff.to_string
        (Tree.Many (diff [List [a; List [b; c]]] [List [a; List [x; y]]]))
    ; print_endline @@ Diff.to_string
        (Tree.Many (diff [List [a; z]] [List [a; List [x; y]]]))
end
