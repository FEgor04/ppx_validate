type t = {
  firstName : string; [@min_length 4] [@max_length 10] [@regex {|[a-zA-Z]+|}]
  age : int; [@min 12] [@max 60]
}
[@@deriving validate, show]

let value = { firstName = "John"; age = 20 }

let show_error = function
  | `FirstName err -> "firstName: " ^ err
  | `Age err -> "age: " ^ err

let show_result = function Ok ok -> show ok | Error err -> show_error err
let () = validate_t value |> show_result |> print_endline
