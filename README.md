Arrow: OCaml testing library based on diffing s-expressions
============================================================

Arrow takes advantage of values that can be converted to s-expressions in order
to present a failing test case as a diff of two s-expressions. It also uses a
ppx preprocessor in order to annotate your tests with source code location
information.

[!img]

Example
-------

Arrow is designed for a setting where:
 * You rely on many assert-equals tests
 * You have mostly pure functions
 * Most of your types can be converted to s-expression

Say, you have the following domain model with `sexp_of_t` functions
generated using [ppx_sexp_conf]() library:

```ocaml
module Pizza = struct
  module Topping =
    type t = Salami | Pineapple
      [@@deriving sexp]
  end

  type t = {
    toppings: Topping.t list;
  } [@@deriving sexp]

  let empty = {toppings=[]}

  let rec remove_pineapple {toppings} = (* faulty function *)
    match toppings with
    | Topping.Pineaplle :: tail -> {toppings=tail}
    | other -> remove_pineapple {toppings=other}
end
```

Now let's write a few tests. First using Arrow with polymorphic comparison:

```ocaml
let (=>) = Arrow.(=>)


let%test "Removing pineapple from empty pizza does nothing" =
  Pizza.remove_pineapple Pizza.empty => Pizza.empty

let%test "Removing pineapple from pizza with no pineapple" =
  Pizza.remove_pineapple Pizza.{toppings=[Topping.Salami]} =>
    Pizza.{toppings=[Topping.Salami]}

let%test "Removing pineapple from pizza w/ pineapple gives pizza w/o one" =
  Pizza.remove_pineapple Pizza.{toppings=[Topping.Pineapple]} =>
    Pizza.{toppings=[]}
```

See the test pass:

```sh
$ jbuilder runtest
...
```

Now let's add a failing test:

```ocaml
let%test "Removing pineapple from pizza w/ many layers of pineapple gives pizza w/o one" =
  Pizza.remove_pineapple Pizza.{toppings=Topping.[Pineapple; Salami; Pineapple]} =>
    Pizza.{toppings=Topping.[Salami]}
```

See tests fail:

```sh
$ jbuilder runtest
...F
_______________________________________________________________________________
./Developer/test_pizza.ml

    14 let%test "Removing pineapple from pizza w/ many layers of pineapple gives pizza w/o one" =
>>> 15   Pizza.remove_pineapple Pizza.{toppings=Topping.[Pineapple; Salami; Pineapple]} =>
    16     Pizza.{toppings=Topping.[Salami]}
_______________________________________________________________________________
```

Now to get the most benefit we must create a custom arrow that can convert
the value under test to s-expression and diff it against the expected value:

```ocaml
let (=>) = Arrow.create Pizza.sexp_of_t
```

Now run again, and see the s-expression diff:

```sh
$ jbuilder runtest
...F
_______________________________________________________________________________
./Developer/test_pizza.ml

    14 let%test "Removing pineapple from pizza w/ many layers of pineapple gives pizza w/o one" =
>>> 15   Pizza.remove_pineapple Pizza.{toppings=Topping.[Pineapple; Salami; Pineapple]} =>
    16     Pizza.{toppings=Topping.[Salami]}

((toppings (Salami -Pineapple)))
_______________________________________________________________________________
```

## Internals

Arrow is using a ppx preprocessor to annotate tests with source code location
information. Given the following test:

```ocaml
let%test "name" =
  let n = 4 in
  1 + 3 => n;
  2 + 2 => n;
  3 + 1 => n
```

It will convert it into the following form:

```ocaml
let () =
  Arrow.Internal.register
    ~file:__FILE__
    ~header:(10, 10) (* Line range with the header `let%test "name" =` *)
    ~body:(11, 14) (* Line range of the body of the test *)
    ~source:[| (* Bundle source; original source file can be unreachable *)
      "let%test \"name\" =";
      "  let n = 4 in";
      "  1 + 3 => n;";
      "  2 + 2 => n;";
      "  3 + 1 => n";
    |]
    ~markers:[|12; 13|] (* Line numbers where tracking markers are set *)
    ~test:(fun () ->
      let n = 4 in
      1 + 3 => n;
      Arrow.Internal.marker 0; (* Indecies into markers array above *)
      2 + 2 => n;
      Arrow.Internal.marker 1;
      3 + 1 => n
    )
```
