open Base

(** Companion observations: exports that enrich primary movements (metadata,
    categories, fees) without being a second transaction stream. The kernel
    stores their raw fields; the adapter owning the format matches and reads
    them. *)

type row =
  { row_id : int64
  ; document_id : int64
  ; institution : string
  ; format : string
  ; row_number : int
  ; fields : string list
  }

(** What a matched row adds to its movement's journal entry. *)
type annotation =
  { value_on : Date.t option
  ; tags : (string * string) list (** Tag names and raw values, in order. *)
  }

type t =
  { format : string
  ; policy : string (** Recorded with every match; changing it re-audits. *)
  ; match_rows :
      row list -> (int64 * Source_event.t) list -> Bank_match.result list Or_error.t
    (** Rows of this format against the institution's own events. *)
  ; annotate : row -> annotation Or_error.t
  ; describe : row -> (string * string option) list Or_error.t
    (** Named fields for listings. *)
  }
