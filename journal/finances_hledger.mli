open Base

(** Deterministic journal serialization through ocaml-hledger. All accounts and
    commodities are declared; commodity precision preserves every stored digit.
    Entry tags retain the SQLite event ID and projection key. *)
val export : Finances.Store.t -> string Or_error.t
