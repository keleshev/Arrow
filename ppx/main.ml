
module Hashtbl = MoreLabels.Hashtbl

module Memoized = struct

  let create ?(size=8) f =
    let table = Hashtbl.create size in
    fun argument ->
      try Hashtbl.find table argument
      with Not_found ->
        let result = f argument in
        Hashtbl.replace table ~key:argument ~data:result;
        result

  let fix ?size f_nonrec x =
    let rec f = lazy (create ?size (fun x -> f_nonrec (Lazy.force f) x)) in
    (Lazy.force f) x
end

open Parsetree

let expression = Ast_helper.Exp.mk

let unit ~loc = expression ~loc (Pexp_construct ({txt=Lident "()"; loc}, None))

let int ~loc n =
  expression ~loc (Pexp_constant (Pconst_integer (string_of_int n, None)))

let int_pair ~loc (x, y) =
  expression ~loc (Pexp_tuple [int ~loc x; int ~loc y])

let rec sequence = function
  | [] -> unit ~loc:Location.none
  | [expr] -> expr
  | {pexp_loc; _} as expr :: tail ->
      expression ~loc:pexp_loc (Pexp_sequence (expr, sequence tail))

let ident = Longident.parse

let call ~loc path params =
  expression ~loc (Pexp_apply (
    expression ~loc (Pexp_ident {txt=ident path; loc}), params))

let array ~loc exprs = expression ~loc (Pexp_array exprs)

module Runtime = struct
  let register ~loc ~test ~header ~body =
    call ~loc "Arrow.Runtime.register" [
      Labelled "file", expression ~loc (Pexp_ident {txt=ident "__FILE__"; loc});
      Labelled "header", int_pair ~loc header;
      Labelled "body", int_pair ~loc body;
      Labelled "source", array ~loc [];
      Labelled "test", test;
    ]
end

let mapper argv =
  { Ast_mapper.default_mapper with structure_item = fun mapper item ->
      match item with
      | {pstr_desc=Pstr_extension (({txt=("test" | "arrow.test"); loc}, payload),
                                   _attrs); pstr_loc} ->
        begin match payload with
        | PStr items ->

            let test_bindings = items |> List.map (function
              | {pstr_desc=Pstr_value (_rec, bindings); _} ->
                  bindings |> List.map (fun x -> x)
              | _ -> failwith "expected structure-level let-bindings (Pstr_value)")
              |> List.flatten
            in
            ();
            let loc = pstr_loc in
            let unit_pattern = {
              ppat_desc=Ppat_construct ({txt=Lident "()"; loc;}, None);
              ppat_loc=loc;
              ppat_attributes=[];
            } in
            let thunk expr = {
              pexp_desc=Pexp_fun (Nolabel, None, unit_pattern, expr);
              pexp_loc=loc;
              pexp_attributes=[];
            } in

            {
              pstr_loc;
              pstr_desc=Pstr_value (Nonrecursive, [{
                pvb_pat=unit_pattern;
                pvb_attributes=[];
                pvb_loc=loc;
                pvb_expr=sequence (test_bindings |> List.map (
                  fun {pvb_pat; pvb_expr; _}  ->
                    Runtime.register ~loc
                      ~test:(thunk pvb_expr)
                      ~header:(pvb_pat.ppat_loc.loc_start.pos_lnum,
                               pvb_pat.ppat_loc.loc_end.pos_lnum)
                      ~body:(pvb_expr.pexp_loc.loc_start.pos_lnum,
                             pvb_expr.pexp_loc.loc_end.pos_lnum)
                  )
                );
              }]);
            }
        | _ -> failwith "hai"
        end
      | other -> Ast_mapper.default_mapper.structure_item mapper item
  }

let () =
  Ast_mapper.register "arrow" mapper
