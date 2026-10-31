open Base

(** A bank movement as the kernel sees it, whatever bank or format it came from.
    Adapters build movements from their own stored events; projection, transfer
    review and journal export read nothing else. *)

type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : Iso4217.t
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
  ; movement_type : Movement_type.t option
  ; reported_code : string option
    (** The bank's own transaction code in a primary source, such as a CSV
            type column; typing rules may map it to a movement type. *)
  ; fields : (string * string) list
    (** Named values the source carries besides the canonical ones, such as an
            OFX name and memo, in source order. *)
  ; main_description : int
    (** Which of [descriptions] the adapter declares as the main text. *)
  }

val create
  :  account:account
  -> own_iban:own_iban
  -> booked_on:Date.t
  -> ?value_on:Date.t
  -> amount:Quantity.t
  -> ?fee:Quantity.t
  -> ?counterparty_iban:Iban.t
  -> ?movement_type:Movement_type.t
  -> ?reported_code:string
  -> ?fields:(string * string) list
  -> ?main_description:int
  -> string list
  -> t Or_error.t

(** Transfer amount excluding the separately recorded fee. *)
val principal : t -> Quantity.t

(** Attach a type derived from the primary source at import. *)
val with_movement_type : t -> Movement_type.t option -> t

(** The movement with its fields replaced. *)
val with_fields : t -> (string * string) list -> t

(** The values a rule can test under a field name. [description] is the main text
    the adapter declares, [text] is every description, and any other name is one
    of {!t.fields}. Descriptions are stripped of surrounding whitespace, and the
    list is empty when the movement has no such field. *)
val field_values : t -> string -> string list
