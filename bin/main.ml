type t = { x : string; [@min_length 2] y : int; z : string }
[@@deriving validate]

let value = { x = "123"; y = 123; z = "123" }
let () = validate_t value |> Result.get_ok |> (fun t -> t.x) |> print_string
