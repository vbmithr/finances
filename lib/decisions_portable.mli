open Base

(** Everything a person decided, as one file: what survives rebuilding the database from
    statements, which come back through [fin import]. Each section is optional, so that a file
    can hold part of this. Movements are named by source identity, not by database ID. *)

type settlement =
  { source : Category_portable.decision
  ; card_institution : string
  ; card_source_account : string
  ; ledger_account : string
  ; reason : string
  }

type reversal =
  { original : Category_portable.decision
  ; reversal : Category_portable.decision
  }

(** A card number assigned to a card account; see docs/cards.md. *)
type card =
  { institution : string
  ; card_reference : string
  ; card_source_account : string
  }

type t =
  { accounts : Bank_account_config.t list option
  ; cards : card list option
  ; categories : string list option
  ; rules : Classification_rule.t list option
  ; assignments : Category_portable.t option
  ; settlements : settlement list option
  ; reversals : reversal list option
  ; exchanges : Exchange_portable.t option
  ; transfers : Transfer_portable.t option
  }

val codec : t Jsont.t
