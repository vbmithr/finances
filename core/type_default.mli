(** A type rule that ships with an institution. It keeps its own name and priority
    when installed into a database, so an update can tell it from a user's rule. *)
type t =
  { institution : string
  ; name : string
  ; priority : int
  ; pattern : string
  ; case_sensitive : bool
  ; account_type : string option
  ; match_type : string option
  ; field : string option (** The field tested; the main description if absent. *)
  ; target : string (** A movement type, or one with {!Movement_type.placeholder}. *)
  }
