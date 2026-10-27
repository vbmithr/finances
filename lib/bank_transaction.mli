open Base

type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : string
  }

type t = private
  { account : account
  ; booked_on : Date.t
  ; amount : Quantity.t
  ; fee : Quantity.t
  ; name : string option
  ; memo : string option
  }

val is_event : Source_event.t -> bool
val of_event : Source_event.t -> t Or_error.t
