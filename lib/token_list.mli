open Base

(** A token list, as wallets and exchanges such as CoW Swap or Uniswap publish them
    (tokenlists.org): which contract on which chain is which token, by the list's account.
    Only what fin reads is kept; logos, tags and extensions are left out. *)

type token =
  { chain_id : int
  ; address : string (** Lower case, as explorers write it. *)
  ; symbol : string
  ; name : string
  }

type t =
  { name : string
  ; version : string (** Such as 7.12.0. *)
  ; tokens : token list
  }

val of_string : string -> t Or_error.t

(** [lookup t] finds a token by chain and contract, whatever the address's case. *)
val lookup : t -> chain_id:int -> address:string -> token option
