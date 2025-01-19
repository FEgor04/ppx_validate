open Ppxlib
module List = ListLabels

let validate_min_length ~loc length continuation =
  let open Ast_builder.Default in
  let length_constant = Pconst_integer (string_of_int length, None) in
  let length_expr = pexp_constant ~loc length_constant in
  [%expr
    let ( let* ) = Result.bind in
    let validate_string_min_length string =
      if String.length string < [%e length_expr] then
        Result.error
        @@ Format.sprintf "value should be at least %d characters long"
             [%e length_expr]
      else Result.ok string
    in
    let* value = validate_string_min_length value in
    [%e continuation]]

let validate_max_length ~loc length continuation =
  let open Ast_builder.Default in
  let length_constant = Pconst_integer (string_of_int length, None) in
  let length_expr = pexp_constant ~loc length_constant in
  [%expr
    let ( let* ) = Result.bind in
    let validate_string_max_length string =
      if String.length string > [%e length_expr] then
        Result.error
        @@ Format.sprintf "value should be at most %d characters long"
             [%e length_expr]
      else Result.ok string
    in
    let* value = validate_string_max_length value in
    [%e continuation]]

let find_int_attribute name ld =
  let ( let* ) = Option.bind in
  let* min_length_attribute =
    List.find_opt ld.pld_attributes ~f:(fun attribute ->
        attribute.attr_name.txt = name)
  in
  let* value_str =
    match min_length_attribute.attr_payload with
    | PStr [%str [%e? exp]] -> (
        match exp.pexp_desc with
        | Pexp_constant (Pconst_integer (value, _)) ->
            Some (int_of_string value)
        | _ -> None)
    | _ -> None
  in
  Some value_str

let find_min_length_attribute = find_int_attribute "min_length"
let find_max_length_attribute = find_int_attribute "max_length"

let string_validators =
  [
    (find_min_length_attribute, validate_min_length);
    (find_max_length_attribute, validate_max_length);
  ]

let validate_string_ld_body ld =
  let loc = ld.pld_loc in
  let expr = [%expr Result.ok value] in
  let validate_expr =
    List.fold_left
      ~f:(fun acc (find_attribute, validator) ->
        match find_attribute ld with
        | None -> acc
        | Some value -> validator ~loc value acc)
      ~init:expr string_validators
  in
  let func = [%expr fun value -> [%e validate_expr]] in
  func

let validate_min ~loc length continuation =
  let open Ast_builder.Default in
  let length_constant = Pconst_integer (string_of_int length, None) in
  let length_expr = pexp_constant ~loc length_constant in
  [%expr
    let ( let* ) = Result.bind in
    let validate_int_min value =
      if value < [%e length_expr] then
        Result.error
        @@ Format.sprintf "value should be at least %d" [%e length_expr]
      else Result.ok value
    in
    let* value = validate_int_min value in
    [%e continuation]]
let find_min_attribute = find_int_attribute "min"

let validate_max ~loc length continuation =
  let open Ast_builder.Default in
  let length_constant = Pconst_integer (string_of_int length, None) in
  let length_expr = pexp_constant ~loc length_constant in
  [%expr
    let ( let* ) = Result.bind in
    let validate_int_max value =
      if value > [%e length_expr] then
        Result.error
        @@ Format.sprintf "value should be at most %d" [%e length_expr]
      else Result.ok value
    in
    let* value = validate_int_max value in
    [%e continuation]]

let find_max_attribute = find_int_attribute "max"



let int_validators = [ (find_min_attribute, validate_min); (find_max_attribute, validate_max) ]

let validate_int_ld_body ld =
  let loc = ld.pld_loc in
  let expr = [%expr Result.ok value] in
  let validate_expr =
    List.fold_left
      ~f:(fun acc (find_attribute, validator) ->
        match find_attribute ld with
        | None -> acc
        | Some value -> validator ~loc value acc)
      ~init:expr int_validators
  in
  let func = [%expr fun value -> [%e validate_expr]] in
  func
