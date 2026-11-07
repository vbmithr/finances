open Base

(** A source account identifier proves an IBAN only when it is one. *)
let own_iban account_id : Movement.own_iban =
  match Iban.of_string account_id with
  | Ok iban -> Known iban
  | Error _ -> Unknown
;;

(** The start of a document, for format detection. *)
let prefix contents = String.prefix contents 4096 |> String.strip

let extension filename = String.lowercase (Stdlib.Filename.extension filename)

let rows_of parse contents ~number ~fields =
  Or_error.map (parse contents) ~f:(List.map ~f:(fun row -> number row, fields row))
;;

let companion_document parse ~number ~fields contents =
  Or_error.map (rows_of parse contents ~number ~fields) ~f:(fun companion_rows ->
    { Document.empty with companion_rows })
;;

let companion_with_row
      ~decode_row
      ~format
      ~policy
      ~input
      ~candidate
      ~match_rows
      ~annotate
      ~describe
      ~header
  : Companion.t
  =
  let row = decode_row in
  { format
  ; policy
  ; display_name = From_annotation
  ; match_rows =
      (fun rows events ->
        let open Or_error.Let_syntax in
        let%bind inputs =
          List.map rows ~f:(fun r ->
            let%map decoded = row r in
            input r decoded)
          |> Or_error.combine_errors
        in
        let%map candidates =
          List.map events ~f:(fun (id, event) -> candidate id event)
          |> Or_error.combine_errors
        in
        match_rows inputs candidates)
  ; annotate =
      (fun r ->
        let open Or_error.Let_syntax in
        let%map decoded = row r in
        annotate decoded)
  ; describe =
      (fun r ->
        let open Or_error.Let_syntax in
        let%map decoded = row r in
        describe decoded)
  ; fields =
      (fun (r : Companion.row) ->
        let open Or_error.Let_syntax in
        let%map header = header r in
        (* Cells beyond the header have no name and are left out. *)
        let n = Int.min (List.length header) (List.length r.fields) in
        List.zip_exn (List.take header n) (List.take r.fields n))
  }
;;

let companion
      ~format
      ~policy
      ~of_fields
      ~input
      ~candidate
      ~match_rows
      ~annotate
      ~describe
      ~header
  =
  companion_with_row
    ~format
    ~policy
    ~input
    ~candidate
    ~match_rows
    ~annotate
    ~describe
    ~header
    ~decode_row:(fun (r : Companion.row) -> of_fields ~row_number:r.row_number r.fields)
;;

(** Package ordinary adapter functions for the registry. *)
let adapter
      ~institution
      ~aliases
      ~kinds
      ?(operation_kinds = [])
      ?(operation_rows =
        fun _ -> Or_error.error_string "this adapter does not interpret operation events")
      ~formats
      ~takes_account_label
      ~overlap
      ~companions
      ~decode
      ~probe
      ~parse
      ()
  : (module Adapter.S)
  =
  (module struct
    let institution = institution
    let aliases = aliases
    let kinds = kinds
    let operation_kinds = operation_kinds
    let operation_rows = operation_rows
    let formats = formats
    let takes_account_label = takes_account_label
    let reports_tickers = false
    let own_symbols = []
    let overlap = overlap
    let companions = companions
    let decode = decode
    let probe = probe
    let parse = parse
  end)
;;

let csv_adapter
      ~institution
      ?(aliases = [])
      ~format
      ~kind
      ~overlap
      ?(takes_account_label = false)
      ?(account_label_error = "CSV import requires --account-id LABEL")
      ?(companions = [])
      ~recognizes
      ~parse
      ~decode
      ()
  : (module Adapter.S)
  =
  let probe ~filename:_ contents = Option.some_if (recognizes contents) format in
  let parse ~(context : Adapter.context) ~format:_ contents =
    if takes_account_label && Option.is_none context.account_label
    then Or_error.error_string account_label_error
    else parse ~context contents
  in
  adapter
    ~institution
    ~aliases
    ~kinds:[ kind ]
    ~formats:[ format ]
    ~takes_account_label
    ~overlap
    ~companions
    ~decode
    ~probe
    ~parse
    ()
;;

let schema_adapter
      ~institution
      ?aliases
      ~format
      ~kind
      ~overlap
      ?(takes_account_label = false)
      ?account_label_error
      ?companions
      ~schema
      ~events
      ~decode
      ()
  : (module Adapter.S)
  =
  csv_adapter
    ~institution
    ?aliases
    ~format
    ~kind
    ~overlap
    ~takes_account_label
    ?account_label_error
    ?companions
    ~recognizes:(Csv_schema.recognizes schema)
    ~parse:(fun ~context contents ->
      let open Or_error.Let_syntax in
      let%bind rows = Csv_schema.parse schema contents in
      let%map events = events ~context rows in
      let doc_context =
        match context.account_label with
        | Some label when takes_account_label -> [ "account_id", label ]
        | _ -> []
      in
      { Document.empty with events; context = doc_context })
    ~decode
    ()
;;

let optional f = Option.map ~f
let date d = Date.to_string d
let quantity q = Quantity.to_string q
