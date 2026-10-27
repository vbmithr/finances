(** Deleting fin's state: a database with its generated outputs and edit draft. *)

(** The files that exist among: [database], its SQLite sidecar files, the
    [bank.journal] beside it, the archives an earlier [fin db reset] left of
    either ([NAME.YYYYMMDDTHHMMSSZ.old]), and its account edit draft. *)
val files : database:string -> string list Base.Or_error.t

(** Deletes [files], then the directories that held them if they are left empty.
    Other files in those directories are kept. *)
val delete : string list -> unit Base.Or_error.t
