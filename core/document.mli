(** What an adapter extracts from one file, before the kernel stores it. Events
    carry the adapter's institution as venue; the kernel checks this. *)

(** A bank-stated period and balances, with the movements the statement lists.
    Calendar dates remain dates; instants use Ptime without an invented timezone. *)
type statement =
  { source_account : string
  ; reference : string option
  ; currency : Iso4217.t
  ; period_start : Temporal.t
  ; period_end : Temporal.t
  ; opening_as_of : Temporal.t
  ; closing_as_of : Temporal.t
  ; opening : Quantity.t
  ; closing : Quantity.t
  ; events : Source_event.t list
  }

(** A balance the source states at a point in time. [account] is the source's own name for
    the account, so that files naming one account differently stay comparable later. *)
type balance =
  { account : string
  ; balance_type : Records.balance_type
  ; commodity : Commodity.t
  ; quantity : Quantity.t
  ; as_of : Temporal.t
  }

type t =
  { statements : statement list
  ; balances : balance list
  ; events : Source_event.t list (** Movements outside any statement. *)
  ; companion_rows : (int * string list) list (** Row number and raw fields. *)
  ; context : (string * string) list
    (** Import hints that are part of the evidence, such as an account label. *)
  }

val empty : t
