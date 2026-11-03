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
    Files ending in [.zip] (case insensitive) are read with Zipc. Members are
    imported in name order as [archive.zip/member]; none are written to disk.
    The read limit applies to compressed bytes and the sum of expanded bytes,
    with at most 1024 members. Hidden and partial member paths are ignored;
    unsafe paths and corrupt/encrypted/unsupported members are read failures.
    Nested archives are not expanded. Recognized members commit independently;
    an archive with no recognized document is reported as unsupported.
    Read failures are persisted and do not prevent unrelated files from being
    processed. Excluded file identities are skipped, including hard links.
    Database failures return Error. *)
val scan
  :  ?account_id:string
  -> ?max_file_bytes:int64
  -> ?exclude:string list
  -> Finances_store.Store.t
  -> institution:string
  -> paths:string list
  -> report Or_error.t
