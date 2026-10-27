open Base

(** ISO text and calendar-day operations on Ptime's date tuples. *)
type t = Ptime.date [@@deriving sexp, compare, equal]

val of_string : string -> t
val to_string : t -> string
val validate_exn : t -> unit
val year : t -> int
val diff : t -> t -> int
val ( < ) : t -> t -> bool
val ( <= ) : t -> t -> bool
val ( > ) : t -> t -> bool
val ( >= ) : t -> t -> bool
