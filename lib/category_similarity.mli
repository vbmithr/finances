open Base

(** A read-only preview based on a reviewed example. *)
type t =
  { seed : Store.bank_review
  ; candidates : Store.bank_review list
  ; conflicts : Store.bank_review list
  }

(** Compare effective descriptions after folding ASCII case and whitespace. *)
val exact_match : Store.bank_review -> Store.bank_review -> bool

(** Match effective descriptions across institutions, ignoring ASCII case and
    whitespace. Reviewed transactions with another category are conflicts;
    confirmed transfers are excluded. *)
val find : seed:int64 -> Store.bank_review list -> t Or_error.t

(** Approximate text matches, ranked by trigram Dice score. Includes
    unclassified and categorized movements, excludes exact matches and transfers.
    Scores are heuristics, not probabilities. *)
val fuzzy
  :  seed:Store.bank_review
  -> Store.bank_review list
  -> (Store.bank_review * float) list
