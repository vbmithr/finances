(** Pure XDG path resolution. Empty or relative XDG overrides are ignored. *)
type t =
  { data_dir : string
  ; config_dir : string
  ; database : string
  ; accounts_config : string
  }

val resolve : home:string -> getenv:(string -> string option) -> t
