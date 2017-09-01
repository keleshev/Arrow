
module Sexp = struct
  type t = Atom of string | List of t list
end
open Sexp

let minimize ~cost (head :: tail) =
  List.fold_left
    (fun left right -> if cost left < cost right then left else right)
    head tail

let sum = List.fold_left (+) 0

let rec count_atoms = function
  | Sexp.Atom _ -> 1
  | Sexp.List list -> sum (List.map count_atoms list)

type t =
  | Addition of Sexp.t
  | Deletion of Sexp.t
  | Equality of Sexp.t
  | Interior of t list

let rec single_cost = function
  | Addition x | Deletion x | Equality x -> count_atoms x
  | Interior list -> sum (List.map single_cost list)

let cost list = single_cost (Interior list)

let rec single_debug = function
  | Addition (Sexp.Atom a) -> "+" ^ a
  | Deletion (Sexp.Atom a) -> "-" ^ a
  | Equality (Sexp.Atom a) -> a
  | Addition x -> "+" ^ "(...)"
  | Deletion x -> "-" ^ "(...)"
  | Equality x -> "(...)"
  | Interior list -> List.fold_left (^) "" (List.map single_debug list)

let debug list = single_debug (Interior list)

let rec diff: Sexp.t list -> Sexp.t list -> 'a = fun left right ->
  match left, right with
  | [], right -> List.map (fun sexp -> Addition sexp) right
  | left, [] -> List.map (fun sexp -> Deletion sexp) left
  | x :: xs, y :: ys when x = y ->
     (* The fact that when x = y we declare it a non-change without considering
        other options makes this diffing algorithm "impatient" (in patdiff sense).
        On one hand this makes our naive algorithm probably linear time in the
        common case of no diff (since we recurse only once), on the other hand
        this might lead to not the best looking diffs, e.g.:
          (a x x x a b b a) vs. (a b b a) => (a -x -x -x -a b b a)
      *)
     Equality x :: diff xs ys
  | Sexp.List ls as x :: xs, (Sexp.List ms as y) :: ys ->
      minimize ~cost [
        Deletion x :: diff xs right;
        Addition y :: diff left ys;
        Interior (diff ls ms) :: diff xs ys;
      ]
  | x :: xs, y :: ys ->
      minimize ~cost [
        Deletion x :: diff xs right;
        Addition y :: diff left ys;
        Deletion x :: Addition y :: diff xs ys;
      ]

module Test: sig end = struct
  let (=>) left right = print_char (if left = right then '.' else 'F')
  let (!) _ = ()

  let pair a b = Sexp.List [a; b]

  let a, b, c = Atom "a", Atom "b", Atom "c"
  let x, y, z = Atom "x", Atom "y", Atom "z"

  let () = !"Cost function counts number of atoms"
    ; cost [Addition a; Deletion b; Equality a; Interior [Equality b]] => 4

  let () = !"Test diff"
    ; diff [] [] => []
    ; diff [] [a; b] => [Addition a; Addition b]
    ; diff [a; b] [] => [Deletion a; Deletion b]
    ; diff [a; b] [a] => [Equality a; Deletion b]
    ; diff [a; b] [a; b] => [Equality a; Equality b]
    ; diff [a; x; a; b; b; a] [a; b; b; a] =>
        [Equality a; Deletion x; Deletion a; Equality b; Equality b; Equality a]
    ; diff [List [a; List [b; c]]] [List [a; List [b]]] =>
        [Interior [Equality a; Interior [Equality b; Deletion c]]]
end
