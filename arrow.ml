
let sum = List.fold_left (+) 0
let (>>) f g x = g (f x)

module Sexp = struct
  type t = Atom of string | List of t list

  let rec size = function
    | Atom _ -> 1
    | List list -> sum (List.map size list)

  let rec to_string = function
    | Atom a -> a
    | List list -> "(" ^ String.concat " " (List.map to_string list) ^ ")"
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

module Direction = struct
  type t = Addition | Deletion | Equality

  let to_string = function Addition -> "+" | Deletion -> "-" | Equality -> ""
end

module Difference = struct
  type t = (Direction.t * Sexp.t) Tree.t
end

let cost' = Tree.fold ~one:(snd >> Sexp.size) ~many:sum
let cost list = cost' (Tree.Many list)

let rec debug = let open Tree in function
  | One (direction, sexp) -> Direction.to_string direction ^ Sexp.to_string sexp
  | Many list -> "(" ^ String.concat " " (List.map debug list) ^ ")"

module Test = struct
  let (=>) left right = print_char (if left = right then '.' else 'F')
  let a, b, c = Sexp.(Atom "a", Atom "b", Atom "c")

  let () = let open Direction in ()
    ; cost' Tree.(Many [
        One (Equality, a);
        One (Deletion, b);
        Many [One (Addition, c)];
      ]) => 3

  let () = let open Direction in ()
    ; debug Tree.(Many [
        One (Equality, a);
        Many [One (Equality, b); One (Deletion, c)];
      ]) => "(a (b -c))"

end

let rec diff: Sexp.t list -> Sexp.t list -> 'a Tree.t list = fun left right ->
  let open Tree in let open Direction in
  match left, right with
  | [], right -> List.map (fun sexp -> One (Addition, sexp)) right
  | left, [] -> List.map (fun sexp -> One (Deletion, sexp)) left
  | x :: xs, y :: ys when x = y ->
     (*
        The fact that when x = y we declare it a non-change without considering
        other options makes this diffing algorithm "impatient" (in patdiff sense).
        On one hand this makes our naive algorithm probably linear time in the
        common case of no diff (since we recurse only once), on the other hand
        this might lead to not the best looking diffs, e.g.:
          (a x x x a b b a) vs. (a b b a) => (a -x -x -x -a b b a)
      *)
     One (Equality, x) :: diff xs ys
  | Sexp.List ls as x :: xs, (Sexp.List ms as y) :: ys ->
      minimize ~cost [
        One (Deletion, x) :: diff xs right;
        One (Addition, y) :: diff left ys;
        Many (diff ls ms) :: diff xs ys;
      ]
  | x :: xs, y :: ys ->
      minimize ~cost [
        One (Deletion, x) :: diff xs right;
        One (Addition, y) :: diff left ys;
        One (Deletion, x) :: One (Addition, y) :: diff xs ys;
      ]

module Test = struct
  let (=>) diffs string =
    Test.(=>) (String.concat " " (List.map debug diffs)) string
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
end
