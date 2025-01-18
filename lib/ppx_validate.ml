open Ppxlib
module List = ListLabels

let field_validate_impl (ld : label_declaration) record_name =
  let open Ast_builder.Default in
  let loc = ld.pld_loc in
  let validator_func_name =
    ppat_var ~loc
      {
        ld.pld_name with
        txt = "validate_" ^ record_name ^ "_" ^ ld.pld_name.txt;
      }
  in
  let func_body =
    match ld.pld_type.ptyp_desc with
    | Ptyp_constr ({ txt = Longident.Lident "int"; _ }, _) ->
        [%expr
          fun value ->
            if value = value then Result.ok value else Result.error "error!!!"]
    | Ptyp_constr ({ txt = Longident.Lident "string"; _ }, _) ->
        [%expr
          fun value ->
            if value = value then Result.ok value else Result.error "error!!!"]
    | _ -> pexp_extension ~loc @@ Location.error_extensionf ~loc "Type is not supported"
  in
  [%stri let [%p validator_func_name] = [%e func_body]]

let fields_return_expressions ~loc (fields : label_declaration list) =
  let open Ast_builder.Default in
  let field_names = List.map fields ~f:(fun field -> field.pld_name.txt) in
  let fields_expr =
    pexp_record ~loc
      (List.map field_names ~f:(fun name ->
           let lid = Located.lident ~loc name in
           (lid, pexp_ident ~loc lid)))
      None
  in
  [%expr Result.ok [%e fields_expr]]

let field_validate_expressions ~loc (fields : label_declaration list)
    record_name =
  let open Ast_builder.Default in
  List.fold_right fields ~init:(fields_return_expressions ~loc fields)
    ~f:(fun field acc ->
      let field_name = field.pld_name.txt in
      let validate_fn =
        pexp_ident ~loc
          (Located.lident ~loc ("validate_" ^ record_name ^ "_" ^ field_name))
      in
      let field_access =
        pexp_field ~loc
          (pexp_ident ~loc (Located.lident ~loc "value"))
          (Located.lident ~loc field_name)
      in
      let pattern = ppat_var ~loc (Located.mk ~loc field_name) in
      [%expr
        let* [%p pattern] = [%e validate_fn] [%e field_access] in
        [%e acc]])

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | {
       ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open;
       ptype_loc;
       _;
      } ->
          let ext =
            Location.error_extensionf ~loc:ptype_loc
              "Cannot derive accessors for non record types"
          in
          [ Ast_builder.Default.pstr_extension ~loc ext [] ]
      | { ptype_kind = Ptype_record fields; _ } ->
          let record_validator =
            [
              (let validator_func_name =
                 Ast_builder.Default.ppat_var ~loc
                   { td.ptype_name with txt = "validate_" ^ td.ptype_name.txt }
               in
               let validator_impl =
                 let bindings =
                   field_validate_expressions ~loc fields td.ptype_name.txt
                 in
                 [%expr
                   fun value ->
                     let ( let* ) = Result.bind in
                     [%e bindings]]
               in
               [%stri let [%p validator_func_name] = [%e validator_impl]]);
            ]
          in
          List.map fields ~f:(fun field ->
              field_validate_impl field td.ptype_name.txt)
          @ record_validator)
  |> List.concat

let () =
  let impl_generator = Deriving.Generator.V2.make_noarg generate_impl in
  Deriving.add "validate" ~str_type_decl:impl_generator |> Deriving.ignore
