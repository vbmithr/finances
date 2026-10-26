open Core

type t =
  { data_dir : string
  ; config_dir : string
  ; database : string
  ; accounts_config : string
  }

let resolve ~home ~getenv =
  let base key fallback =
    match getenv key with
    | Some value when not (Filename.is_relative value) -> value
    | _ -> Filename.concat home fallback
  in
  let data_dir = Filename.concat (base "XDG_DATA_HOME" ".local/share") "fin" in
  let config_dir = Filename.concat (base "XDG_CONFIG_HOME" ".config") "fin" in
  { data_dir
  ; config_dir
  ; database = Filename.concat data_dir "fin.sqlite3"
  ; accounts_config = Filename.concat config_dir "accounts.json"
  }
;;
