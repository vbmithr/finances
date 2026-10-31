open Base

(** Everything fin stores about one movement: where it was imported from, the
    source's own fields, the companion rows matched to it, and the decisions
    recorded for it. Plain data built by [Store.show_movement]. *)

type document =
  { id : int64
  ; sha256 : string
  ; imported_at : string
  ; paths : string list (** The files this document was read from, oldest first. *)
  }

type statement =
  { reference : string option
  ; source_account : string
  ; currency : string
  ; period : string
  ; opening : string
  ; closing : string
  }

type companion =
  { row_id : int64
  ; format : string
  ; row_number : int
  ; document : document
  ; fields : (string * string) list (** Every raw column, by header name. *)
  ; state : string
  ; reason : string
  ; policy : string
  ; reported : (string * Movement_type.t option) option
    (** The value the deciding type rule matched, and the type it gave. *)
  ; display_name : string option
  ; tags : (string * string) list
  }

type t =
  { explanation : Explanation.t
  ; venue : string
  ; account : string
  ; kind : string
  ; source_id : string
  ; occurred : string
  ; retrieved_at : string
  ; payload : string (** The source event's payload, indented when it is JSON. *)
  ; fields : (string * string) list
    (** Every column or tag the source carried, by name, as stored beside it. *)
  ; documents : document list
  ; statements : statement list
  ; companions : companion list
  ; transfer : (int64 * int64 * string) option
    (** The confirmed transfer pair this movement belongs to, and its reason. *)
  }
