(** What kind of institution holds the money. It decides how an institution is
    described and grouped; it does not change how its files are read. Brokers are
    not modelled yet. *)

type network =
  | Evm of { chain_id : int }
  | Other of string
  | Unspecified
[@@deriving compare, equal]

type t =
  | Bank
  | Crypto_exchange
  | Wallet of network
  (** An address on a network, read from a block explorer's exports. *)
[@@deriving compare, equal]

(** The token used in definitions and JSON: [bank], [crypto-exchange] or [wallet]. *)
val to_string : t -> string

(** The words used in terminal output: [bank], [crypto exchange] or [wallet]. *)
val label : t -> string

(** The string codec does not carry network metadata; [wallet] decodes as [Unspecified]. *)
val kind_jsont : t Jsont.t
