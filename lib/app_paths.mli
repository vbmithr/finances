(** Pure XDG data path resolution. Empty or relative overrides are ignored. *)
type t =
  { data_dir : string
  ; database : string
  }

val resolve : home:string -> getenv:(string -> string option) -> t
