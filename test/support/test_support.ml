open Base
open Finances
open Finances_store

(** A second OFX institution, registered only in tests, to check that
    identities are scoped by institution. *)
module Other_bank = struct
  include Ofx_bank

  let institution = "other-bank"
  let aliases = []
  let operation_kinds = []
  let operation_rows _ = Or_error.error_string "OFX events are bank movements"
  let parse ~context:_ ~format:_ contents = Ofx_bank.parse ~institution contents
end

let registry =
  Registry.create
    ~type_rules:
      (* The rules that name a column (Wise and Revolut); BoursoBank's description
         rules are not installed, so its tests start from untyped movements. *)
      (List.filter Builtin.type_rules ~f:(fun r -> Option.is_some r.Type_default.field))
    (Builtin.adapters @ [ (module Other_bank : Adapter.S) ])
  |> Or_error.ok_exn
;;

(** Imports through the normal path; a rejected or unsupported file is an
    error, as the caller expects evidence to be stored. *)
let import ?account_label db ~institution ~path contents =
  let open Or_error.Let_syntax in
  let%bind (report : Bank_import.file_report) =
    Import.import_bank_file ?account_label db ~institution ~path ~contents
  in
  match report.outcome with
  | Imported | Unchanged -> Ok report
  | Rejected | Unsupported | Unreadable ->
    Or_error.error_string (Option.value report.message ~default:"import refused")
;;

let import_ofx db ~institution contents =
  import db ~institution ~path:"statement.ofx" contents
;;
