open Base
module Db = Rel_sqlite3
open Handle

(** Immutable contexts attached to accepted documents for an institution. *)
val document_contexts : t -> institution:string -> string list Or_error.t

(** Saved, explicitly reviewed regular-expression rules. *)
val classification_rules : t -> Classification_rule.t list Or_error.t

(** Import one readable inbox snapshot and record its outcome. The institution's
    adapter recognizes and parses the file; a file another bank recognizes is
    rejected with a pointer to that institution. Unsupported and rejected bytes
    are archived, too. Successful evidence and its attempt commit together; a
    source conflict rolls back before a separate rejection is saved. *)
val import_bank_file
  :  ?account_label:string
  -> t
  -> institution:string
  -> path:string
  -> contents:string
  -> Bank_import.file_report Or_error.t

val record_bank_read_error
  :  t
  -> institution:string
  -> path:string
  -> message:string
  -> Bank_import.file_report Or_error.t

val bank_import_history : ?limit:int -> t -> Bank_import.file_report list Or_error.t
val decode_fields : string -> string list Or_error.t
val registered_companions : ?institution:string -> t -> (string * Companion.t) list

val add_classification_rule
  :  Handle.t
  -> Classification_rule.t
  -> (unit, Error.t) Result.t

val replace_classification_rule
  :  Handle.t
  -> Classification_rule.t
  -> (unit, Error.t) Result.t

val delete_classification_rule : Handle.t -> String.t -> (unit, Error.t) Result.t

val set_classification_rule_mode
  :  Handle.t
  -> String.t
  -> Classification_rule.mode
  -> (unit, Error.t) Result.t

val rename_classification_rule
  :  Handle.t
  -> old_name:String.t
  -> new_name:String.t
  -> (unit, Error.t) Result.t

val set_institution_definition
  :  ?dry_run:bool
  -> Handle.t
  -> name:string
  -> definition:String.t
  -> expected:String.t Option.t
  -> expected_rules:Classification_rule.t List.t
  -> rules:Classification_rule.t List.t
  -> source_unchanged:bool
  -> (int, Error.t) Result.t
