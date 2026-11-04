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
  let operation_group _ = None
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

(** A Revolut statement as a records file naming another account, for a second account of one
    currency: a records file can say which account it is, and a Revolut export cannot. *)
let revolut_account ~account contents =
  let definition =
    List.find_exn (Lazy.force Csv_converter.bundled) ~f:(fun d ->
      String.equal (Csv_converter.institution d) "revolut")
  in
  let t =
    Csv_converter.convert definition ~name:"revolut.csv" contents |> Or_error.ok_exn
  in
  let id currency = account ^ "-" ^ currency in
  Records.to_string
    { t with
      accounts =
        List.map t.accounts ~f:(fun (a : Records.account) ->
          { a with id = id a.id; number = Some account })
    ; records =
        List.map t.records ~f:(fun (r : Records.record) ->
          { r with account = id r.account })
    }
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
