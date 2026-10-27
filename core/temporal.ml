type t =
  | Date of Date.t
  | Instant of Ptime.t

let to_string = function
  | Date date -> Date.to_string date
  | Instant time -> Timestamp.to_rfc3339 time
;;

let of_string_exn text =
  if String.length text = 10
  then Date (Date.of_string text)
  else Instant (Timestamp.of_rfc3339_exn text)
;;
