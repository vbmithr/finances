(** What kind of institution holds the money. It decides how an institution is
    described and grouped; it does not change how its files are read. Brokers are
    not modelled yet. *)

type t =
  | Bank
  | Crypto_exchange
[@@deriving compare, equal]

(** The token used in definitions and JSON: [bank] or [crypto-exchange]. *)
val to_string : t -> string

(** The words used in terminal output: [bank] or [crypto exchange]. *)
val label : t -> string

val jsont : t Jsont.t
