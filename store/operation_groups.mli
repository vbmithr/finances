open Base

(** Which stored operations read as one operation. Every operation belongs to
    exactly one group; an operation named by no link is alone in its group. Groups
    are derived on read and never change what is stored. *)
type t

(** Each link is [(canonical, member)]. The canonical operation keeps its ID when
    the group is read. An operation named by two links is an error. *)
val of_links : (int64 * int64) list -> t Or_error.t

(** The ID the group of this operation reads under. *)
val canonical : t -> int64 -> int64

(** All operations of this operation's group, canonical first. *)
val members : t -> int64 -> int64 list
