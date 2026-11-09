open Base
open Finances_store
open Or_error.Let_syntax
open Cli

let builtin_institution_name registry name =
  List.exists (Registry.institutions registry) ~f:(fun institution ->
    List.mem (Registry.aliases registry institution) name ~equal:String.equal)
;;

let ensure_generic_source database institution expected_kind =
  with_database database (fun db ->
    let%bind companion = Setup.institution_companion_profile db institution in
    let%bind () =
      if String.equal expected_kind "csv-movement" && Option.is_some companion
      then
        Or_error.error_string
          "this institution uses a CSV companion; import it with fin import INSTITUTION \
           PATH"
      else Ok ()
    in
    let%bind events = Ledger.events db in
    if
      List.exists events ~f:(fun (_, event) ->
        String.equal event.Source_event.venue institution
        && List.mem [ "csv-movement"; "bank-transaction" ] event.kind ~equal:String.equal
        && not (String.equal event.kind expected_kind))
    then
      Or_error.error_string
        "this institution already has imports in another generic format; CSV and OFX are \
         separate movement sources and may duplicate transactions"
    else Ok ())
;;
