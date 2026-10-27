(** Deleting fin's state: a database with everything fin keeps beside it, and
    optionally the account configuration. *)

(** The files that exist among: [database], its SQLite sidecar files, the
    [bank.journal] beside it, the archives an earlier [fin db reset] left of
    either ([NAME.YYYYMMDDTHHMMSSZ.old]), and [config] when given. *)
val files : database:string -> config:string option -> string list Base.Or_error.t

(** Deletes [files], then the directories that held them if they are left empty.
    Other files in those directories are kept. *)
val delete : string list -> unit Base.Or_error.t
