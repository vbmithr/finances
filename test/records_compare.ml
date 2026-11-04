open Base
open Stdio

(* Imports real files through an institution's old adapter and through records, into databases
   and journals under a directory, to compare what the move would change. A development tool; it
   needs a user's statements, so no test runs it. *)

let usage = "records_compare INSTITUTION OUTPUT-DIRECTORY PATH..."

let definition institution =
  List.find (Lazy.force Csv_converter.bundled) ~f:(fun d ->
    String.equal (Csv_converter.institution d) institution)
;;

let sources institution : Adapter.overlap * Records_adapter.source list =
  let csv =
    Option.map (definition institution) ~f:(Records_adapter.of_definition "csv")
  in
  let structured : Records_adapter.source option =
    match institution with
    | "boursobank" ->
      Some
        { format = "ofx"
        ; recognizes = Ofx_converter.recognizes
        ; convert = Ofx_converter.convert ~institution
        ; tags = []
        }
    | "bunq" ->
      Option.map
        (Builtin.mt940_profile institution)
        ~f:(fun profile : Records_adapter.source ->
          { format = "mt940"
          ; recognizes = Mt940_converter.recognizes profile
          ; convert = Mt940_converter.convert profile
          ; tags = []
          })
    | _ -> None
  in
  match institution with
  | "wise" ->
    ( Source_ids
    , [ { format = "wise"
        ; recognizes = Wise_converter.recognizes
        ; convert = Wise_converter.convert
        ; tags =
            Option.value_map
              (definition institution)
              ~default:[]
              ~f:Csv_converter.journal_tags
        }
      ] )
  | _ ->
    let overlap : Adapter.overlap =
      match structured, definition institution with
      | Some { format = "ofx"; _ }, _ -> Source_ids
      | Some _, _ -> Day_inventory
      | None, Some definition -> Csv_converter.overlap definition
      | None, None -> Source_ids
    in
    overlap, List.filter_opt [ structured; csv ]
;;

let registry institution =
  let overlap, sources = sources institution in
  let others =
    List.filter Builtin.adapters ~f:(fun (module A : Adapter.S) ->
      not (String.equal A.institution institution))
  in
  Registry.create
    ~fallback:Builtin.fallback
    ~fallbacks:Builtin.fallbacks
    ~type_rules:Builtin.type_rules
    ~source_evidence:Builtin.source_evidence
    (Records_adapter.make ~institution ~overlap sources :: others)
  |> Or_error.ok_exn
;;

let run name registry ~institution ~out ~paths =
  let path file = Stdlib.Filename.concat out (name ^ file) in
  match
    Finances_import.Workflow.run
      ~registry
      ?account_id:
        (match Registry.find registry institution with
         | Some (module A : Adapter.S) when A.takes_account_label -> Some "main"
         | _ -> None)
      ~database:(path ".sqlite3")
      ~institution
      ~paths
      ~journal:(path ".journal")
      ()
  with
  | Error error -> printf "%s failed: %s\n" name (Error.to_string_hum error)
  | Ok report ->
    let companions = report.companions in
    let outcomes =
      List.map report.inputs.files ~f:(fun file ->
        Finances.Bank_import.outcome_name file.outcome)
      |> List.sort ~compare:String.compare
      |> List.group ~break:(fun a b -> not (String.equal a b))
      |> List.map ~f:(fun group ->
        Printf.sprintf "%s=%d" (List.hd_exn group) (List.length group))
      |> String.concat ~sep:" "
    in
    printf
      "%s: %s inserted=%d matched=%d unmatched=%d ambiguous=%d conflicts=%d\n"
      name
      outcomes
      (List.sum (module Int) report.inputs.files ~f:(fun file -> file.inserted))
      companions.matched
      companions.unmatched
      companions.ambiguous
      companions.conflicts
;;

let () =
  match Array.to_list (Sys.get_argv ()) with
  | _ :: institution :: out :: (_ :: _ as paths) ->
    (try Stdlib.Sys.mkdir out 0o700 with
     | Sys_error _ -> ());
    run "old" Builtin.registry ~institution ~out ~paths;
    run "new" (registry institution) ~institution ~out ~paths
  | _ -> prerr_endline usage
;;
