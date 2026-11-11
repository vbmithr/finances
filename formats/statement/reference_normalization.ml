open Base

let valid_hex ~length text =
  String.length text = length
  && String.for_all text ~f:(function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false)
;;

let normalize_hex ~prefix ~label ~length ~description reference =
  match String.chop_prefix reference ~prefix with
  | Some id ->
    let id = String.filter id ~f:(fun c -> not (Char.equal c '-')) in
    if not (valid_hex ~length id) then failwith "invalid normalized reference";
    prefix ^ String.lowercase id
  | None
    when valid_hex ~length reference
         && String.Caseless.equal (String.strip description) label ->
    prefix ^ String.lowercase reference
  | None -> reference
;;
