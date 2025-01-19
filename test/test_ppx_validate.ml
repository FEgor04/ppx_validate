type t = {
  firstName: string[@min_length 4][@max_length 12];
  age: int[@min 18][@max 120];
}[@@deriving validate, show]

let%expect_test "invalid minLength" =
  let value = validate_t { firstName="abc"; age = 20 } in
  value |> Result.get_error |> print_endline;
  [%expect {| value should be at least 4 characters long |}];;

let%expect_test "invalid maxLength" =
  let value = validate_t { firstName="more than twevle characters"; age = 20 } in
  value |> Result.get_error |> print_endline;
  [%expect {| value should be at most 12 characters long |}];;

let%expect_test "valid" =
  let value = validate_t { firstName="abcde"; age = 20 } in
  value |> Result.get_ok |> show |> print_endline;
  [%expect {| { Test_ppx_validate.firstName = "abcde"; age = 20 } |}];;


