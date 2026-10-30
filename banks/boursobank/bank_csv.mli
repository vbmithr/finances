open Base

(** The observed BoursoBank semicolon CSV layout, in UTF-8 with optional BOM.
    The first "Solde" column is the movement amount; the second is a reported
    account balance. CSV observations do not supply transaction identities. *)
type row = Csv_companion_layout.row

val profile : Csv_companion_layout.t
val recognizes : string -> bool
val parse : string -> row list Or_error.t
val of_fields : row_number:int -> string list -> row Or_error.t
