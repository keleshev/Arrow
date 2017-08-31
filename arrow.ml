let (=>) left right = print_char (if left = right then '.' else 'F')




module Sexp = struct
  type t = Atom of string | List of t list
end

module Difference = struct


  module Direction = struct
    type t = Addition | Deletion
  end

  module Minor = struct
    type t =
      | One of Direction.t * Sexp.t
      | Many of t list
  end

  type t =
    (* `addition` and `deletion` cannot be equal, cannot be both lists *)
    | Major of {addition: Sexp.t; deletion: Sexp.t}
    | Minor of Minor.t


end






let rec diff: Sexp.t list -> Sexp.t list -> 'a = fun left right ->
  match left, right with
  | [], right -> List.map (fun sexp -> `Addition sexp) right
  | left, [] -> List.map (fun sexp -> `Deletion sexp) left
  | x :: xs, y :: ys ->
      if x = y then `Equality x :: diff xs ys else []
  | _ -> assert false







let pair a b = Sexp.List [a; b]

let hai, bye = Sexp.(Atom "hai", Atom "bye")

open Difference

let (!) _ = ()

let () = !"No difference between equal atoms"
  ; diff [] [hai; bye] => [`Addition hai; `Addition bye]
  ; diff [hai; bye] [] => [`Deletion hai; `Deletion bye]
  ; diff [] [] => []
  ; diff [hai; bye] [hai] => [`Equality hai; `Deletion bye]

(*
let () = !"No difference between equal lists"
  ; diff (pair hai bye) (pair hai bye) => None

let () = !"Major difference between to different atoms"
  ; diff hai bye => Some (Major {addition=hai; deletion=bye})

let () = !"Major difference between an atom and a list"
  ; diff hai (pair hai bye) =>
      Some (Major {addition=hai; deletion=(pair hai bye)})
  ; diff (pair hai bye) hai =>
      Some (Major {addition=(pair hai bye); deletion=hai})


(*

foo => bar                +foo -bar
(foo bar) => (foo)        (foo +bar)

*)*)
