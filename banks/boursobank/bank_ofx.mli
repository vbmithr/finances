open Base

(** OFX 2.x XML bank statements. Amounts are exact and dates retain calendar
    semantics. OFX 1.x SGML and credit-card/investment message sets are rejected. *)
type account = private
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : Iso4217.t
  }

(** [user_date] is the unparsed DTUSER field, not a validated value date. The
    observed BoursoBank export contains invalid dates there. [source_payload]
    includes account, currency and canonical transaction XML as JSON, omitting
    DTUSER; the exact original document is retained separately. *)
type transaction = private
  { fitid : string
  ; booked_on : Date.t
  ; user_date : string option
  ; amount : Quantity.t
  ; transaction_type : string
  ; name : string option
  ; memo : string option
  ; source_payload : string
  }

type balance = private
  { amount : Quantity.t
  ; as_of : string
  }

type statement = private
  { account : account
  ; transactions : transaction list
  ; ledger_balance : balance option
  ; available_balance : balance option
  }

type t = private
  { contents : string
  ; statements : statement list
  }

val parse : string -> t Or_error.t

(** Collision-free identity for the bank's account fields, not a ledger account. *)
val account_identity : account -> string

(** Reconstruct a transaction from its canonical SQLite source event. Validates
    that the payload agrees with the event's identity and booking date. DTUSER
    is available only in the archived document, not in this canonical payload. *)
val source_transaction : Source_event.t -> (account * transaction) Or_error.t
