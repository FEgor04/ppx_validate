type t = {
  firstName : string;
      [@min_length 4] [@max_length 12] [@lowercase_ascii] [@regex "^[a-z]+$"]
  age : int; [@min 18] [@max 120]
}
[@@deriving validate, show]

let error_to_string = function
  | `Age err -> "age: " ^ err
  | `FirstName err -> "firstName: " ^ err

let%expect_test "invalid minLength" =
  let value = validate_t { firstName = "abc"; age = 20 } in
  value |> Result.get_error |> error_to_string |> print_endline;
  [%expect {| firstName: value should be at least 4 characters long |}]

let%expect_test "invalid maxLength" =
  let value = validate_t { firstName = "morethantwevlecharacters"; age = 20 } in
  value |> Result.get_error |> error_to_string |> print_endline;
  [%expect {| firstName: value should be at most 12 characters long |}]

let%expect_test "invalid regex" =
  let value = validate_t { firstName = "abcde!"; age = 20 } in
  value |> Result.get_error |> error_to_string |> print_endline;
  [%expect {| firstName: value should satisfy regex ^[a-z]+$ |}]

let%expect_test "valid" =
  let value = validate_t { firstName = "ABCde"; age = 20 } in
  value |> Result.get_ok |> show |> print_endline;
  [%expect {| { Test_ppx_validate.firstName = "abcde"; age = 20 } |}]

let%expect_test "invalid min int" =
  let value = validate_t { firstName = "abcde"; age = 10 } in
  value |> Result.get_error |> error_to_string |> print_endline;
  [%expect {| age: value should be at least 18 |}]

let%expect_test "invalid max int" =
  let value = validate_t { firstName = "abcde"; age = 150 } in
  value |> Result.get_error |> error_to_string |> print_endline;
  [%expect {| age: value should be at most 120 |}]
