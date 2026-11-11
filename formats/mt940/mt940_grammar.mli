open Base

(** A syntax reader for common MT940 statement exports. Original field text and
    unknown fields are retained; bank-specific interpretation belongs elsewhere.
    This is not a complete SWIFT network-message validator. *)
type direction =
  | Credit
  | Debit
  | Reversal_credit
  | Reversal_debit

type field =
  { tag : string
  ; text : string
  }

type entry =
  { raw : string
  ; value_on : Date.t
  ; entry_on : (int * int) option
  ; direction : direction
  ; funds_code : char option
  ; amount : Quantity.t
  ; transaction_type : string
  ; reference : string
  ; bank_reference : string option
  ; supplementary : string option
  ; memo : string option
  }

type balance =
  { tag : string
  ; date : Date.t
  ; currency : Iso4217.t
  ; direction : direction
  ; amount : Quantity.t
  }

type statement =
  { reference : string
  ; account : string
  ; number : string
  ; opening : balance option
  ; closing : balance option
  ; entries : entry list
  ; fields : field list
  }

type t = statement list

(** Parse terminated statements. Field 86 after an entry is optional; unknown
    fields and unassociated field 86 values remain in [fields]. *)
val parse : string -> t Or_error.t

val signed : direction -> Quantity.t -> Quantity.t
