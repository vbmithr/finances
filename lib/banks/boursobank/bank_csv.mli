open Core

(** The observed BoursoBank semicolon CSV layout, in UTF-8 with optional BOM.
    The first "Solde" column is the movement amount; the second is a reported
    account balance. CSV observations do not supply transaction identities. *)
type row = private
  { row_number : int
  ; account_id : string
  ; booked_on : Date.t
  ; value_on : Date.t option
  ; amount : Bigdecimal.t
  ; description : string
  ; suggested_description : string option
  ; category : string option
  ; parent_category : string option
  ; comment : string option
  ; account_label : string option
  ; reported_balance : Bigdecimal.t option
  ; pointage : string option
  ; fields : string list
  }

val recognizes : string -> bool
val parse : string -> row list Or_error.t
val of_fields : row_number:int -> string list -> row Or_error.t
