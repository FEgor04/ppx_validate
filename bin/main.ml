type t = { x : string; [@min_length 4][@max_length 8] y : int; z : string }
[@@deriving validate, show]

let value = { x = "12345"; y = 123; z = "123" }
let show_result = function Ok ok -> show ok | Error err -> err
let () = validate_t value |> show_result |> print_endline
