open Base

type t =
  { data_dir : string
  ; database : string
  }

let resolve ~home ~getenv =
  let base key fallback =
    match getenv key with
    | Some value when not (Stdlib.Filename.is_relative value) -> value
    | _ -> Stdlib.Filename.concat home fallback
  in
  let data_dir = Stdlib.Filename.concat (base "XDG_DATA_HOME" ".local/share") "fin" in
  { data_dir; database = Stdlib.Filename.concat data_dir "fin.sqlite3" }
;;
