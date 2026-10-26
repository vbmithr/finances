open Core

type ignored =
  { path : string
  ; reason : string
  }

type report =
  { files : Finances.Bank_import.file_report list
  ; ignored : ignored list
  }

(** Recursively import regular files, in path order. Files are not modified.
    Hidden paths, partial-file names, and symbolic links are reported as ignored.
    The default per-file limit is 64 MiB. Read failures are persisted and do not
    prevent unrelated files from being processed. Excluded file identities are
    skipped, including hard links. Database failures return Error. *)
val scan
  :  ?account_id:string
  -> ?max_file_bytes:int64
  -> ?exclude:string list
  -> Finances.Store.t
  -> institution:string
  -> directory:string
  -> report Or_error.t
