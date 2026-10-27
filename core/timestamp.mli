(** RFC 3339 parsing and exact integer conversion through Ptime. *)
val of_rfc3339_exn : string -> Ptime.t

(** Legacy Wise source-ID encoding: nonnegative Int64 nanoseconds, truncating
    finer fractions. Not the representation used in the model or SQLite. *)
val nanoseconds_since_epoch_exn : Ptime.t -> int64

(** Canonical UTC text preserving Ptime's picosecond precision. *)
val to_rfc3339 : Ptime.t -> string

(** Exact conversion at an external Unix-millisecond boundary. *)
val of_unix_ms_exn : int64 -> Ptime.t
