open Base

val is_zip : string -> bool

(** Decode metadata only. Reject more than 1024 members or an expanded byte total
    above [max_bytes], before any decompression or import. *)
val decode : max_bytes:int64 -> string -> Zipc.Member.t list Or_error.t

val valid_path : string -> bool
val ignored_path : string -> bool

(** Decompress and verify the member CRC and expanded size. *)
val contents : Zipc.File.t -> string Or_error.t
