open Base

(** Card numbers, as institutions print them, and the card accounts they belong to. A card
    account's own number is one of its cards; a person can assign others, such as a
    replaced card's. See docs/cards.md. *)

type t =
  { institution : string
  ; card_reference : string
  ; known_by : string option (** ["statement"] or ["assigned"]; [None] when unassigned. *)
  ; card_source_account : string option
  ; account_id : string option
  ; ledger_account : string option
  ; bank_rows : int (** Bank rows naming the card: its payments, and any refund. *)
  ; first_seen : Date.t option
  ; last_seen : Date.t option
  ; settled : int
  ; undecided : int
  ; purchases_from : Date.t option (** The card account's first movement of its own. *)
  }

(** Every card seen on a bank row or as a card account's own number. *)
val list : Handle.t -> t list Or_error.t

(** A card as printed, or by a unique ending such as its last digits. *)
val resolve : Handle.t -> string -> t Or_error.t

type assignment =
  { card : t
  ; card_source_account : string
  ; account_id : string
  ; ledger_account : string
  }

(** What [assign] would record, after every check; [None] when it is already so. *)
val preview_assign
  :  Handle.t
  -> card:string
  -> account_id:string
  -> assignment option Or_error.t

val assign : Handle.t -> assignment -> unit Or_error.t

(** Refuses a card that is its account's own number, or one that settlements were recorded
    through. *)
val preview_unassign : Handle.t -> card:string -> t Or_error.t

val unassign : Handle.t -> t -> unit Or_error.t

(** Assignments as [(institution, card_reference, card_source_account)], for decisions. *)
val assignments : Handle.t -> (string * string * string) list Or_error.t

(** Records an exported assignment, checking it as [assign] does; [false] when it is already
    recorded. *)
val restore_assignment
  :  Handle.t
  -> institution:string
  -> card_reference:string
  -> card_source_account:string
  -> bool Or_error.t
