open Base

val is_zip : string -> bool

(** Decode metadata only. Reject more than 1024 members or an expanded byte total
    above [max_bytes], before any decompression or import. *)
val decode : max_bytes:int64 -> string -> Zipc.Member.t list Or_error.t

val valid_path : string -> bool
val ignored_path : string -> bool

(** Decompress and verify the member CRC and expanded size. *)
val contents : Zipc.File.t -> string Or_error.t

(** The files of an archive that can be read, in name order, as member path and
    contents. Hidden and partial-file members are left out, as in a directory walk.
    An unsafe path, a corrupt or encrypted member and a nested archive are errors
    naming the member, and so is an expanded total above [max_bytes]. *)
val files : max_bytes:int64 -> string -> (string * string) list Or_error.t
