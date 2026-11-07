open Base

(** A fiat currency, a crypto asset, or a symbol reported by a provider that is not yet
    resolved to either. An asset is one asset whichever network holds it: USDC on Ethereum
    and USDC on Polygon are both [Asset "USDC"], and the network stays evidence of the
    movement. Provider symbols are not globally unique, so only a reviewed resolution or a
    definition's list of known contracts makes one an asset. *)
type t = private
  | Fiat of Iso4217.t
  | Asset of string
  | Reported of
      { provider : string
      ; symbol : string
      }

val fiat : Iso4217.t -> t

(** [asset symbol] refuses an ISO currency code, which names fiat. *)
val asset : string -> t Or_error.t

(** [of_code code] is fiat for an ISO currency code, else the asset [code]. *)
val of_code : string -> t Or_error.t

val reported : provider:string -> symbol:string -> t Or_error.t
val provider : t -> string option
val symbol : t -> string option

(** The commodity the journal names: a fiat code or an asset; [None] for a reported symbol,
    which must be resolved first. *)
val journal_symbol : t -> string option

val display : t -> string
val equal : t -> t -> bool
val codec : t Jsont.t
