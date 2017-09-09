
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let getenv s = try Sys.getenv s with Not_found -> ""

let expression ~loc desc = {pexp_desc=desc; pexp_loc=loc; pexp_attributes=[]}

let unit ~loc = expression ~loc (Pexp_construct ({txt=Lident "()"; loc}, None))

let rec sequence = function
  | [] -> unit ~loc:Location.none
  | [expr] -> expr
  | {pexp_desc; pexp_loc; _} as expr :: tail ->
      expression ~loc:pexp_loc (Pexp_sequence (expr, sequence tail))

let ident = Longident.parse

let call ~loc path params =
  expression ~loc (Pexp_apply (
    expression ~loc (Pexp_ident {txt=ident path; loc}), params))

module Runtime = struct
  let register ~loc ~test =
    call ~loc "Arrow.Runtime.register" [
      Labelled "file", expression ~loc (Pexp_ident {txt=ident "__FILE__"; loc});
      Labelled "test", test;
    ]
end

let getenv_mapper argv =
  { default_mapper with structure_item = fun mapper item ->
      match item with
      | {pstr_desc=Pstr_extension (({txt=("test" | "arrow.test"); loc}, payload),
                                   _attrs); pstr_loc} ->
        begin match payload with
        | PStr items ->

            let test_expressions = items |> List.map (function
              | {pstr_desc=Pstr_value (_rec, bindings); _} ->
                  bindings |> List.map (fun {pvb_expr; pvb_pat; _} -> pvb_expr)
              | _ -> failwith "expected structure-level let-bindings (Pstr_value)")
              |> List.flatten
            in
            ();
            let _array exprs = {
              pexp_desc=Pexp_array exprs;
              pexp_loc=loc;
              pexp_attributes=[];
            } in
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

            let test_thunks = List.map thunk test_expressions in
            {
              pstr_loc;
              pstr_desc=Pstr_value (Nonrecursive, [{
                pvb_pat=unit_pattern;
                pvb_attributes=[];
                pvb_loc=loc;
                pvb_expr=sequence (test_thunks |> List.map (fun thunk ->
                  (Runtime.register ~loc ~test:thunk)
                ));
              }]);
            }
        | _ -> failwith "hai"
        end
      | other -> default_mapper.structure_item mapper item
  }

let () = register "getenv" getenv_mapper
