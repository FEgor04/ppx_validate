# ppx_validate
ppx_validate is an OCaml preprocessor library that derives type validation functions

## How it works

### Examples

Given the following code

```ocaml
type t = {
  firstName : string; [@min_length 4] [@max_length 10]
  age : int; [@min 18] [@max 60]
}
[@@deriving validate]
```

`ppx_validate` will generate the following 😱 _Monad_-based 😎  validator:
```ocaml
    let validate_t_firstName value =
      let ( let* ) = Result.bind in
      let validate_field value =
        if (String.length value) > 10
        then
          Result.error
            (`FirstName
               (Format.sprintf "value should be at most %d characters long"
                  10))
        else Result.ok value in
      let* value = validate_field value
       in
      let ( let* ) = Result.bind in
      let validate_field value =
        if (String.length value) < 4
        then
          Result.error
            (`FirstName
               (Format.sprintf "value should be at least %d characters long"
                  4))
        else Result.ok value in
      let* value = validate_field value
       in Result.ok value
    let _ = validate_t_firstName
    let validate_t_age value =
      let ( let* ) = Result.bind in
      let validate_field value =
        if value > 60
        then
          Result.error
            (`Age (Format.sprintf "value should be at most %d" 60))
        else Result.ok value in
      let* value = validate_field value
       in
      let ( let* ) = Result.bind in
      let validate_field value =
        if value < 18
        then
          Result.error
            (`Age (Format.sprintf "value should be at least %d" 18))
        else Result.ok value in
      let* value = validate_field value
       in Result.ok value
    let _ = validate_t_age
    let validate_t value =
      let ( let* ) = Result.bind in
      let* firstName = validate_t_firstName value.firstName
       in let* age = validate_t_age value.age
           in Result.ok { firstName; age }
```

Which allows use to use it like that:
```ocaml
let value_raw = { firstName="John"; age = 10 }
let value = validate_t value_raw (** Result.err `age "value should be at least 18" *)
```
