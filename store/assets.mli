open Base

(** What fin maps a provider's symbol to: a token contract on a network, or an exchange's
    ticker, is resolved once to fiat or an asset for every movement reporting it. See
    docs/operations.md. *)

type decision =
  | Unmapped
  | Mapped of Commodity.t
  | Ignored (** Spam: its movements stay as evidence and never count. *)

type t =
  { provider : string
  ; symbol : string
  ; movements : int
  ; first_seen : Date.t option
  ; last_seen : Date.t option
  ; name : string option (** The name its rows most often give it, as stated. *)
  ; decision : decision
  }

(** Every reported symbol, and every mapping, by provider and symbol. *)
val list : Handle.t -> t list Or_error.t

(** [find db ~provider ~symbol] is the symbol a user names, exactly or by a unique start, such
    as the first characters of a contract address. *)
val find : Handle.t -> provider:string -> symbol:string -> t Or_error.t

(** [decide db asset decision] records it, refusing to change a symbol whose confirmed
    operations rest on its current commodity. [false] when nothing changes. *)
val decide : Handle.t -> t -> decision -> bool Or_error.t

(** Mappings and ignored symbols, for decisions export: [None] is ignored. *)
val mappings : Handle.t -> (string * string * Commodity.t option) list Or_error.t

(** Restores one exported mapping; a different existing one is refused. [false] when it was
    already there. *)
val restore
  :  Handle.t
  -> provider:string
  -> symbol:string
  -> Commodity.t option
  -> bool Or_error.t
