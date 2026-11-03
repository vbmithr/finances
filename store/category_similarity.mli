open Base

(** A read-only preview based on a reviewed example. *)
type t =
  { seed : Review.bank_review
  ; candidates : Review.bank_review list
  ; conflicts : Review.bank_review list
  }

(** Compare effective descriptions after folding ASCII case and whitespace. *)
val exact_match : Review.bank_review -> Review.bank_review -> bool

(** Match effective descriptions across institutions, ignoring ASCII case and
    whitespace. Reviewed transactions with another category are conflicts;
    confirmed transfers are excluded. *)
val find : seed:int64 -> Review.bank_review list -> t Or_error.t

(** Approximate text matches, ranked by trigram Dice score. Includes
    unclassified and categorized movements, excludes exact matches and transfers.
    Scores are heuristics, not probabilities. *)
val fuzzy
  :  seed:Review.bank_review
  -> Review.bank_review list
  -> (Review.bank_review * float) list
