(** RFC 3339 parsing and exact integer conversion through Ptime. *)
val of_rfc3339_exn : string -> Ptime.t

val nanoseconds_since_epoch_exn : Ptime.t -> int64
