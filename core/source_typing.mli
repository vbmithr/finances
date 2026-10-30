open Base

type t

val codec : t Jsont.t
val of_json : string -> t Or_error.t
val to_json : t -> string
val institution : t -> string
val empty : string -> t Or_error.t
val apply : t -> Movement.t -> Movement.t
