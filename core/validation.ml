open Base

let text label s =
  if
    String.is_empty s
    || String.exists s ~f:(fun c -> Char.to_int c < 32 || Char.equal c '\127')
  then Or_error.errorf "%s must be nonempty and contain no control characters" label
  else Ok s
;;

let token label s =
  let valid = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | _ -> false
  in
  if (not (String.is_empty s)) && String.for_all s ~f:valid
  then Ok s
  else
    Or_error.errorf "%s must contain only letters, digits, underscores or hyphens" label
;;

let account s =
  let parts = String.split s ~on:':' in
  if List.for_all parts ~f:(fun p -> Result.is_ok (token "account component" p))
  then Ok s
  else Or_error.error_string "account must be colon-separated nonempty tokens"
;;
