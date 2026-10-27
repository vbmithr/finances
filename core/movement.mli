open Base

(** A bank movement as the kernel sees it, whatever bank or format it came from.
    Adapters build movements from their own stored events; projection, transfer
    review and journal export read nothing else. *)

type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : string
  }

(** The IBAN the movement's own account is known by in the source evidence.
    [Unknown] when the source identifies the account otherwise (a label or a
    domestic number); a registered IBAN may then supply it. [Not_applicable]
    for accounts that have no IBAN, such as payment cards. *)
type own_iban =
  | Known of Iban.t
  | Unknown
  | Not_applicable

type t = private
  { account : account
  ; own_iban : own_iban
  ; booked_on : Date.t
  ; value_on : Date.t option
  ; amount : Quantity.t (** Signed net change of the account balance. *)
  ; fee : Quantity.t (** Non-negative, posted separately. *)
  ; descriptions : string list (** Free texts, in source order. *)
  ; counterparty_iban : Iban.t option
  }

val create
  :  account:account
  -> own_iban:own_iban
  -> booked_on:Date.t
  -> ?value_on:Date.t
  -> amount:Quantity.t
  -> ?fee:Quantity.t
  -> ?counterparty_iban:Iban.t
  -> string list
  -> t Or_error.t

(** Transfer amount excluding the separately recorded fee. *)
val principal : t -> Quantity.t
