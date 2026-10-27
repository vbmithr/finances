open Base
open Finances

(** A second OFX institution, registered only in tests, to check that
    identities are scoped by institution. *)
module Other_bank = Ofx_bank.Make (struct
    let institution = "other-bank"
    let aliases = []
  end)

let registry =
  Registry.create (Builtin.adapters @ [ (module Other_bank : Adapter.S) ])
  |> Or_error.ok_exn
;;

(** Imports through the normal path; a rejected or unsupported file is an
    error, as the caller expects evidence to be stored. *)
let import ?account_label db ~institution ~path contents =
  let open Or_error.Let_syntax in
  let%bind (report : Bank_import.file_report) =
    Store.import_bank_file ?account_label db ~institution ~path ~contents
  in
  match report.outcome with
  | Imported | Unchanged -> Ok report
  | Rejected | Unsupported | Unreadable ->
    Or_error.error_string (Option.value report.message ~default:"import refused")
;;

let import_ofx db ~institution contents =
  import db ~institution ~path:"statement.ofx" contents
;;
