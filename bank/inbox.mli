open Base

type ignored =
  { path : string
  ; reason : string
  }

type report =
  { files : Finances.Bank_import.file_report list
  ; ignored : ignored list
  }

(** Import [paths] in the order given: a regular file is imported, a directory is
    walked recursively in name order. Files are not modified. [paths] must each be
    a regular file or a directory, not a symbolic link. Inside directories, hidden
    paths, partial-file names and symbolic links are reported as ignored; explicit
    paths are always processed. A file or directory reached twice is imported once
    and then reported as already listed. The default per-file limit is 64 MiB.
    Read failures are persisted and do not prevent unrelated files from being
    processed. Excluded file identities are skipped, including hard links.
    Database failures return Error. *)
val scan
  :  ?account_id:string
  -> ?max_file_bytes:int64
  -> ?exclude:string list
  -> Finances.Store.t
  -> institution:string
  -> paths:string list
  -> report Or_error.t
