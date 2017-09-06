
module Sexp = Arrow.Sexp
module Diff = Arrow.Diff
module Tree = Arrow.Tree
module Change = Arrow.Change

let (=>) left right = print_char (if left = right then '.' else 'F')

let a, b, c = Sexp.(Atom "a", Atom "b", Atom "c")
let x, y, z = Sexp.(Atom "x", Atom "y", Atom "z")

let () =
  Sexp.to_string (List [a; List [b; c]; List [a; List [b]]]) =>
    "(a (b c) (a (b)))"

let () = let open Change in
  Diff.cost Tree.(Many [
    One (Equality a);
    One (Deletion b);
    Many [One (Addition c)];
  ]) => 3

let () = let open Change in
  Diff.debug Tree.(Many [
    One (Equality a);
    Many [One (Equality b); One (Deletion c)];
  ]) => "(a (b -c))"

let (=>) diff string = Diff.debug diff => string


let diff = Diff.create

let () = ()
  ; diff a a => "a"
  ; diff a b => "-a +b"
  ; diff (List [a]) b => "-(a) +b"
  ; diff a (List [b]) => "-a +(b)"
  ; diff (List []) (List []) => "()"
  ; diff (List [a]) (List [a]) => "(a)"
  ; diff (List []) (List [a]) => "(+a)"
  ; diff (List [a]) (List []) => "(-a)"
  ; diff (List [a; b]) (List [a]) => "(a -b)"
  ; diff (List [a]) (List [a; b]) => "(a +b)"
  ; diff (List [a; x; a; b; b; a]) (List [a; b; b; a]) => "(a -x -a b b a)"

let () = print_endline ""
  ; print_endline @@ Diff.to_string
      (diff (List [a; List [b; c]]) (List [a; List [x; y]]))
  ; print_endline @@ Diff.to_string
      (diff (List [a; z]) (List [a; List [x; y]]))
