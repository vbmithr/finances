open Base

(** Interpret the profile-shaped statements currently used by Bunq. Generic
    MT940 parsing is provided by [Mt940_grammar]. *)
type reader

val make : Mt940_profile.t -> reader
val account_id : reader -> Mt940_grammar.statement -> string
val currency : reader -> Mt940_grammar.statement -> Iso4217.t
val opening_on : reader -> Mt940_grammar.statement -> Date.t
val closing_on : reader -> Mt940_grammar.statement -> Date.t
val opening : reader -> Mt940_grammar.statement -> Quantity.t
val closing : reader -> Mt940_grammar.statement -> Quantity.t

(** The transactions of one statement, with the booked date resolved from the
    entry date and the value date. *)
val entries : reader -> Mt940_grammar.statement -> Mt940_transaction.t list Or_error.t
