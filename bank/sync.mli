open Core

type report =
  { inbox : Inbox.report
  ; mapping_changes : int
  ; projection : Finances.Store.projection_report
  ; csv : Finances.Store.csv_report
  ; bunq_csv : Finances.Store.csv_report
  ; wise_csv : Finances.Store.csv_report
  ; journal : string
  }

(** Validate control paths, scan, apply optional mappings, project and atomically
    publish the journal. Valid files commit independently of rejected files.
    Source imports survive a later mapping/projection/output failure; the previous
    journal is preserved. TMPDIR must share a filesystem with the destination. *)
val run
  :  ?account_id:string
  -> ?accounts:string
  -> ?max_file_bytes:int64
  -> database:string
  -> institution:string
  -> directory:string
  -> journal:string
  -> unit
  -> report Or_error.t
