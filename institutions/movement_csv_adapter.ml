open Base

(** A movement-CSV profile as a registry adapter, so these institutions import
    through the same probe/parse/ingest pipeline as every other venue. Each CSV
    row becomes one source event of the profile's payload kind; the proposed
    operation is re-derived from the stored event's fields at ingest time. *)
let make (profile : Movement_csv.profile) () : (module Adapter.S) =
  let recognizes contents =
    Csv_reader.recognizes ~separator:',' ~trim:false ~header:profile.header contents
  in
  let probe ~filename:_ contents =
    Option.some_if (recognizes contents) profile.payload_tag
  in
  let parse ~context:_ ~format:_ contents =
    let open Or_error.Let_syntax in
    let%bind events = Movement_csv.events profile contents in
    let%bind () =
      if List.is_empty events
      then Or_error.errorf "%s file has no transaction rows" profile.institution
      else Ok ()
    in
    let identities =
      List.map events ~f:(fun (e : Source_event.t) ->
        e.venue, e.account, e.kind, e.source_id)
    in
    if List.contains_dup identities ~compare:[%compare: string * string * string * string]
    then
      Or_error.error_string "identical source rows are ambiguous; no rows can be imported"
    else Ok { Document.empty with events }
  in
  Adapter_support.adapter
    ~institution:profile.institution
    ~aliases:profile.aliases
    ~kinds:[]
    ~operation_kinds:[ profile.payload_tag ]
    ~operation_rows:(Movement_csv.operation_rows_of_event profile)
    ~formats:[ profile.payload_tag ]
    ~takes_account_label:false
    ~overlap:Adapter.Source_ids
    ~companions:[]
    ~decode:(fun _ ->
      Or_error.error_string
        (profile.institution ^ " rows are operations, not bank movements"))
    ~probe
    ~parse
    ()
;;
