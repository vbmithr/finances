type time =
  | Date of Date.t
  | Instant of Ptime.t

type statement =
  { source_account : string
  ; reference : string option
  ; currency : Iso4217.t
  ; period_start : time
  ; period_end : time
  ; opening_as_of : time
  ; closing_as_of : time
  ; opening : Quantity.t
  ; closing : Quantity.t
  ; events : Source_event.t list
  }

type t =
  { statements : statement list
  ; events : Source_event.t list
  ; companion_rows : (int * string list) list
  ; context : (string * string) list
  }

let empty = { statements = []; events = []; companion_rows = []; context = [] }

let time_to_string = function
  | Date date -> Date.to_string date
  | Instant time -> Timestamp.to_rfc3339 time
;;

let time_of_string_exn text =
  if String.length text = 10
  then Date (Date.of_string text)
  else Instant (Timestamp.of_rfc3339_exn text)
;;
