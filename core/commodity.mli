open Base

(** A fiat currency or a symbol reported by a provider whose canonical asset
    identity is not yet known. Provider symbols are not globally unique. *)
type t = private
  | Fiat of Iso4217.t
  | Onchain_native of { network : string }
  | Onchain_token of
      { network : string
      ; contract : string
      ; token_id : string option
      }
  | Reported of
      { provider : string
      ; symbol : string
      }

val fiat : Iso4217.t -> t
val reported : provider:string -> symbol:string -> t Or_error.t
val onchain_native : network:string -> t Or_error.t

val onchain_token
  :  network:string
  -> contract:string
  -> token_id:string option
  -> t Or_error.t

val provider : t -> string option
val symbol : t -> string option
val display : t -> string
val equal : t -> t -> bool
val codec : t Jsont.t
