open Base

(** A read-only preview based on a reviewed example. *)
type t =
  { seed : Store.bank_review
  ; candidates : Store.bank_review list
  ; conflicts : Store.bank_review list
  }

(** Match card merchants when both are available, otherwise match descriptions,
    after ASCII case folding and whitespace normalization, within the same
    institution. Reviewed transactions with a different
    category are returned as conflicts; confirmed transfers are excluded. *)
val find : seed:int64 -> Store.bank_review list -> t Or_error.t
