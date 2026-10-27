open Base

type row = private
  { row_number : int
  ; account_id : string
  ; booked_on : Date.t
  ; value_on : Date.t
  ; amount : Quantity.t
  ; counterparty : string
  ; name : string
  ; description : string
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
val match_rows : input list -> (int64 * Bunq_transaction.t) list -> Bank_match.result list
