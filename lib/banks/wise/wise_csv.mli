open Core

type row = private
  { row_number : int
  ; reference : string
  ; booked_local : string
  ; amount : Bigdecimal.t
  ; currency : string
  ; description : string
  ; running_balance : Bigdecimal.t
  ; fees : Bigdecimal.t
  ; payment_reference : string option
  ; merchant : string option
  ; payee : string option
  ; fields : string list
  }

type input =
  { row_id : int64
  ; document_id : int64
  ; row : row
  }

val recognizes : string -> bool
val parse : string -> row list Or_error.t
val of_fields : row_number:int -> string list -> row Or_error.t

val match_rows
  :  input list
  -> (int64 * Wise_camt.transaction) list
  -> Bank_match.result list
