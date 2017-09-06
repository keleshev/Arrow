let sum = List.fold_left (+) 0
let (>>) f g x = g (f x)
let const x _ = x

let fprintf = Format.fprintf
let format_list list = Format.pp_print_list ~pp_sep:Format.pp_print_space list

module Color = struct
  let escape = Format.sprintf "\027[%dm"

  let create code format_child f child =
    fprintf f "%s%a%s" (escape code) format_child child (escape 0)

  let red c = create 31 c
  let green c = create 32 c
  let yellow c = create 33 c
end

module Sexp = struct
  type t = Base.Sexp.t = Atom of string | List of t list

  let rec fold ~atom ~list = function
    | Atom a -> atom a
    | List l -> list (List.map (fold ~atom ~list) l)

  let size = fold ~atom:(const 1) ~list:sum

  let rec format f = function
    | Atom a -> fprintf f "%s" a
    | List l -> fprintf f "@[<hv 1>(%a)@]" (format_list format) l

  let to_string = Format.asprintf "%a" format
end

let minimize ~cost (head :: tail) =
  let folder left right = if cost left <= cost right then left else right in
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
        fprintf f "%a" (Color.green Sexp.format) sexp
    | Deletion sexp ->
        fprintf f "%a" (Color.red Sexp.format) sexp
    | Mutation {deletion; addition} ->
        fprintf f "%a %a" format (Deletion deletion) format (Addition addition)

  let to_string = Format.asprintf "%a" format

  let rec debug_format f = function
    | Equality sexp ->
        fprintf f "%a" Sexp.format sexp
    | Addition sexp ->
        fprintf f "+%a" Sexp.format sexp
    | Deletion sexp ->
        fprintf f "-%a" Sexp.format sexp
    | Mutation {deletion; addition} ->
        fprintf f "%a %a"
          debug_format (Deletion deletion)
          debug_format (Addition addition)

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
  let cost_list list = cost (Tree.Many list)

  let rec debug = let open Tree in function
    | One change -> Change.debug change
    | Many list -> "(" ^ String.concat " " (List.map debug list) ^ ")"

  open Change
  open Tree
  open Sexp

  let rec create: Sexp.t -> Sexp.t -> Change.t Tree.t = fun left right ->
    match left, right with
    | x, y when x = y -> One (Equality x)
    | List xs, List ys -> Many (diff_list xs ys)
    | deletion, addition -> One (Mutation {deletion; addition})

  and diff_list: Sexp.t list -> Sexp.t list -> Change.t Tree.t list =
    fun left right ->
    match left, right with
    | [], right -> List.map (fun sexp -> One (Addition sexp)) right
    | left, [] -> List.map (fun sexp -> One (Deletion sexp)) left
    | x :: xs, y :: ys when x = y ->
        (* When x = y we match impatiently (think patdiff) in order to optimize
           our naive algorith in the common case of little diff *)
        One (Equality x) :: diff_list xs ys
    | (Sexp.List ls as x) :: xs, (Sexp.List ms as y) :: ys ->
        minimize ~cost:cost_list [
          One (Deletion x) :: diff_list xs right;
          One (Addition y) :: diff_list left ys;
          Many (diff_list ls ms) :: diff_list xs ys;
        ]
    | x :: xs, y :: ys ->
        minimize ~cost:cost_list [
          One (Deletion x) :: diff_list xs right;
          One (Addition y) :: diff_list left ys;
          One (Mutation {deletion=x; addition=y}) :: diff_list xs ys;
        ]
end
