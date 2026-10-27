(** Moving a database aside so the next command starts a fresh one. *)

type moved =
  { from : string
  ; to_ : string
  }

(** Renames [database], its SQLite sidecar files ([-journal], [-wal], [-shm]) and the
    [bank.journal] generated beside it, adding a [.YYYYMMDDTHHMMSSZ.old] suffix from
    [now] (UTC). Files that do not exist are skipped, so the result is empty when
    there is nothing to move. Nothing is deleted, an existing name is never
    overwritten, and the account configuration is not touched. *)
val archive : now:Ptime.t -> database:string -> moved list Base.Or_error.t
