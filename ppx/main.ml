
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

module Ppx = struct

  let read_lines filename = (* TODO: tail rec, handle exceptions *)
    let rec go channel =
      match input_line channel with
      | line -> line :: go channel
      | exception End_of_file -> []
    in
    Array.of_list (go (open_in filename))

  let read_lines_memoized = Memoized.create read_lines

  open Parsetree

  module Pattern = struct
    let create = Ast_helper.Pat.mk

    let unit ~loc = create ~loc (Ppat_construct ({txt=Lident "()"; loc}, None))
  end

  module Expression = struct
    let create = Ast_helper.Exp.mk

    let constructor ~loc name =
      create ~loc (Pexp_construct ({txt=Longident.parse name; loc}, None))

    let ident ~loc name =
      create ~loc (Pexp_ident {txt=Longident.parse name; loc})

    let unit ~loc = constructor ~loc "()"
    let string ~loc s = create ~loc (Pexp_constant (Pconst_string (s, None)))

    let int ~loc n =
      create ~loc (Pexp_constant (Pconst_integer (string_of_int n, None)))

    let int_pair ~loc (x, y) = create ~loc (Pexp_tuple [int ~loc x; int ~loc y])

    let thunk ~loc expr =
      create ~loc (Pexp_fun (Nolabel, None, Pattern.unit ~loc, expr))

    let rec sequence = function
      | [] -> unit ~loc:Location.none
      | [expr] -> expr
      | {pexp_loc; _} as expr :: tail ->
          create ~loc:pexp_loc (Pexp_sequence (expr, sequence tail))

    let call ~loc path params =
      create ~loc (Pexp_apply (ident ~loc path, params))

    let array ~loc exprs = create ~loc (Pexp_array exprs)
  end

  module Runtime = struct
    let register ~loc ~test ~header ~body =
      let (start, _), (_, finish) = header, body in
      let length = finish - start in
      let filename = loc.Location.loc_start.pos_fname in
      (* TODO: What if this filename is different from the __FILE__ below? How? *)
      let all_lines = read_lines_memoized filename in
      (* TODO: Array.sub can raise *)
      let lines = Array.to_list (Array.sub all_lines (start - 1) (length + 1)) in
      let module E = Expression in
      let source = List.map (E.string ~loc) lines in
      E.call ~loc "Arrow.Runtime.register" [
        Labelled "file", E.ident ~loc "__FILE__";
        Labelled "header", E.int_pair ~loc header;
        Labelled "body", E.int_pair ~loc body;
        Labelled "source", E.array ~loc source;
        Labelled "thunk", test;
      ]
  end

  module Mapper = struct
    let structure_item mapper = function
      | {
          pstr_desc=Pstr_extension (({
            txt=("test" | "arrow.test");
	    (* Location of the extension point itself (%test) is probably the
	       best place to point the user to in case of problems with the
               generated code. *)
            loc;
          }, payload), _);
          _;
        } ->
        begin match payload with
        | PStr items ->

            let test_bindings = items |> List.map (function
              | {pstr_desc=Pstr_value (_rec, bindings); _} -> bindings
              | _ ->
                  failwith "expected structure-level let-bindings (Pstr_value)")
              |> List.flatten
            in
            {
              pstr_loc=loc;
              pstr_desc=Pstr_value (Nonrecursive, [{
                pvb_pat=Pattern.unit ~loc;
                pvb_attributes=[];
                pvb_loc=loc;
                pvb_expr=Expression.sequence (test_bindings |> List.map (
                  fun {pvb_pat; pvb_expr; _}  ->
                    Runtime.register ~loc
                      ~test:(Expression.thunk ~loc pvb_expr)
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
      | other -> Ast_mapper.default_mapper.structure_item mapper other

    let it argv = Ast_mapper.{default_mapper with structure_item}
  end

  let () = Ast_mapper.register "arrow" Mapper.it
end
