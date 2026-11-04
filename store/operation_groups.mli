open Base

(** Which stored operations read as one. Every operation belongs to exactly one
    group; an operation in no group is alone. Groups are derived on read and never
    change what is stored. *)
type t

(** [links] are confirmed decisions as [(canonical, member)]; the canonical operation
    keeps its ID when the group is read. [stated] lists groups an institution's own
    records state, with the first as canonical. A person's decision outranks the
    source: an operation in a link leaves its stated group. An operation named by two
    links is an error. *)
val create : links:(int64 * int64) list -> stated:int64 list list -> t Or_error.t

(** The ID the group of this operation reads under. *)
val canonical : t -> int64 -> int64

(** Noncanonical member IDs paired with their canonical IDs, in member order. *)
val noncanonical_members : t -> (int64 * int64) list

(** All operations of this operation's group, canonical first. *)
val members : t -> int64 -> int64 list
