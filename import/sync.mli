open Base

type report =
  { inbox : Inbox.report
  ; identity_changes : int
  ; projection : Finances.Store.projection_report
  ; companions : Finances.Store.companion_report
  ; journal : string
  }

(** Validate control paths, scan, use saved account settings, project and atomically
    publish the journal. Valid files commit independently of rejected files.
    Source imports survive a later account/projection/output failure; the previous
    journal is preserved. TMPDIR must share a filesystem with the destination. *)
val run
  :  registry:Finances_core.Registry.t
  -> ?account_id:string
  -> ?configure_new_accounts:
       (Finances.Store.t -> Finances.Store.bank_account_info list -> int Or_error.t)
       (** Called after import, before projection, with only accounts
      first seen in this run. Returns the number of audited IBAN registrations. *)
  -> ?max_file_bytes:int64
  -> database:string
  -> institution:string
  -> paths:string list
  -> journal:string
  -> unit
  -> report Or_error.t
