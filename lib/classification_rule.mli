open Base

type action =
  | Category of string
  | Settlement of string
  | Type of Movement_type.t
  | Type_and_category of Movement_type.t * string
  | Card_reference
  | Withdrawal_reversal_hint

type type_condition =
  | Unknown
  | Is of Movement_type.t

type mode =
  | Automatic
  | Review
  | Disabled

type t = private
  { mode : mode
  ; account_type : string option
  ; priority : int
  ; name : string
  ; pattern : string option
  ; institution : string option
  ; match_type : type_condition option
  ; case_sensitive : bool
  ; action : action
  ; field : string option
    (** The field a type rule tests: [text] for any description, a named one such as
        [name], [memo] or [reference], a column of a CSV primary, or a column of a
        matched companion file, all by their names; the main description when
        absent. It must be automatic. Other rules have none. *)
  }

val create
  :  mode:mode
  -> account_type:string option
  -> priority:int
  -> name:string
  -> pattern:string option
  -> institution:string option
  -> match_type:type_condition option
  -> case_sensitive:bool
  -> action:action
  -> ?field:string
  -> unit
  -> t Or_error.t

val action_name : action -> string
val condition_name : type_condition -> string
val condition_of_string : string -> type_condition
val action_target : action -> string
val action_type : action -> Movement_type.t option
val action_of_fields : string -> string -> string option -> action Or_error.t
