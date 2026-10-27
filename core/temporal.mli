(** A calendar date or an exact instant. Dates assert neither a time of day
    nor a timezone; they are never implicitly converted to instants. *)
type t =
  | Date of Date.t
  | Instant of Ptime.t

(** ISO date text or canonical UTC RFC 3339 preserving picosecond precision. *)
val to_string : t -> string

(** Parse ISO dates or RFC 3339 instants, preserving the distinction.
    Raises on invalid input. *)
val of_string_exn : string -> t
