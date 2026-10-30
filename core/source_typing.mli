open Base

type t

type rule = private
  { description : string
  ; account_type : string option
  ; case_sensitive : bool
  ; kind : Movement_type.t
  ; match_type : string option
  ; compiled : Re.re
  }

val with_match_type : rule -> string option -> rule
val rules : t -> rule list
val create : institution:string -> rule list -> t
val make_rule : string -> string option -> bool -> string -> rule Or_error.t
val codec : t Jsont.t
val of_json : string -> t Or_error.t
val to_json : t -> string
val institution : t -> string
val empty : string -> t Or_error.t
val apply : t -> Movement.t -> Movement.t
