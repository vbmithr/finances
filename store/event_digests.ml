open Base
open Handle

type t =
  { payload : string
  ; fields : string
  ; evidence : string
  }

let sha256 text = Digestif.SHA256.(to_hex (digest_string text))

let raw_fields_hash (e : Source_event.t) =
  List.map e.fields ~f:(fun (name, value) ->
    Printf.sprintf "%d:%s%d:%s" (String.length name) name (String.length value) value)
  |> String.concat
  |> sha256
;;

let compute (e : Source_event.t) =
  let identity = Records.row_identity e.payload in
  { payload = sha256 (Option.value identity ~default:e.payload)
  ; fields =
      (match identity with
       | Some identity -> sha256 identity
       | None -> raw_fields_hash e)
  ; evidence =
      (if List.is_empty e.fields
       then "payload:" ^ sha256 e.payload
       else "fields:" ^ raw_fields_hash e)
  }
;;

let store db event_id e =
  let d = compute e in
  db_result
    (Db.exec
       db.conn
       (Rel_sql.Stmt.(
          func
            "INSERT INTO \
             source_event_digests(event_id,payload_sha256,fields_sha256,evidence) VALUES \
             (?,?,?,?)"
            (int64 @-> text @-> text @-> text @-> unit))
          event_id
          d.payload
          d.fields
          d.evidence))
;;

let load db =
  let* rows =
    collect
      db
      (statement
         "SELECT event_id,payload_sha256,fields_sha256,evidence FROM source_event_digests"
         Db_schema.Source_event_digests.row)
  in
  Ok
    (Hashtbl.of_alist_exn
       (module Int64)
       (List.map rows ~f:(fun (r : Db_schema.Source_event_digests.t) ->
          ( r.event_id
          , { payload = r.payload_sha256
            ; fields = r.fields_sha256
            ; evidence = r.evidence
            } ))))
;;
