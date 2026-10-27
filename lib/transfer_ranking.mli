open Base

type edge =
  { first : int64
  ; second : int64
  ; weight : int
  }

type assignment =
  { edge : edge
  ; component : int
  ; best_score : int option
  ; exclusion_margin : int option
  ; inclusion_penalty : int option
  ; status : string
  }

type evidence =
  { code : string
  ; points : int
  ; detail : string
  }

type ranked =
  { candidate : Store.transfer_candidate
  ; score : int
  ; evidence : evidence list
  ; assignment : assignment
  ; confidence : string
  }

val policy : string
val max_component_vertices : int

(** Exact maximum-weight matching with unmatched vertices allowed. Supported
    edges occur in every optimum; equal optima remain alternatives. Oversized
    components are explicitly not evaluated. Scores are not probabilities. *)
val assignments : edge list -> assignment list Or_error.t

val analyze : Store.transfer_candidate list -> ranked list Or_error.t
