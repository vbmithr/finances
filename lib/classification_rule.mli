open Base

type action =
  | Category of string
  | Settlement of string

type t = private
  { name : string
  ; pattern : string
  ; institution : string option
  ; case_sensitive : bool
  ; action : action
  }

val create
  :  name:string
  -> pattern:string
  -> institution:string option
  -> case_sensitive:bool
  -> action:action
  -> t Or_error.t

val action_name : action -> string
val action_target : action -> string
val action_of_fields : string -> string -> action Or_error.t
