open Finances_store
open Base

type report =
  { inputs : Files.report
  ; projection : Ledger.projection_report
  ; companions : Ledger.companion_report
  ; journal : string
  }

(** Validate control paths, scan, use saved account settings, project and atomically
    publish the journal. Valid files commit independently of rejected files.
    Source imports survive a later account/projection/output failure; the previous
    journal is preserved. TMPDIR must share a filesystem with the destination. *)
val run
  :  registry:Finances_core.Registry.t
  -> ?account_id:string
  -> ?max_file_bytes:int64
  -> database:string
  -> institution:string
  -> paths:string list
  -> journal:string
  -> unit
  -> report Or_error.t
