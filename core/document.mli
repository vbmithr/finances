(** What an adapter extracts from one file, before the kernel stores it. Events
    carry the adapter's institution as venue; the kernel checks this. *)

(** A bank-stated period and balances, with the movements the statement lists.
    Dates are the bank's text: a calendar date or a timestamp with offset. *)
type statement =
  { source_account : string
  ; reference : string option
  ; currency : string
  ; period_start : string
  ; period_end : string
  ; opening_as_of : string
  ; closing_as_of : string
  ; opening : Quantity.t
  ; closing : Quantity.t
  ; events : Source_event.t list
  }

type t =
  { statements : statement list
  ; events : Source_event.t list (** Movements outside any statement. *)
  ; companion_rows : (int * string list) list (** Row number and raw fields. *)
  ; context : (string * string) list
    (** Import hints that are part of the evidence, such as an account label. *)
  }

val empty : t
