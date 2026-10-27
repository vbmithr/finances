(** What an adapter extracts from one file, before the kernel stores it. Events
    carry the adapter's institution as venue; the kernel checks this. *)

(** A bank-stated period and balances, with the movements the statement lists.
    Calendar dates remain dates; instants use Ptime without an invented timezone. *)
type time =
  | Date of Date.t
  | Instant of Ptime.t

type statement =
  { source_account : string
  ; reference : string option
  ; currency : Iso4217.t
  ; period_start : time
  ; period_end : time
  ; opening_as_of : time
  ; closing_as_of : time
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
val time_to_string : time -> string
val time_of_string_exn : string -> time
