open Base

(* Records as source events, so that everything after import works as it does for any adapter.
   A record states one balance effect in a currency, which is a bank movement, or anything else,
   which is an operation: several legs, a reported commodity, or no effect at all.

   Not stored yet, though kept in the archived file: balances, and a statement that is not a
   period with an opening and a closing. Observations are refused until pairing exists. *)

let movement_kind = "records-movement"
let operation_kind = "records-operation"

let account_type (account : Records.account) =
  Option.value account.account_type ~default:"current"
;;

let account_identity ~institution (account : Records.account) =
  Json.encode (Jsont.list Jsont.string) [ institution; account.id; account_type account ]
;;

let fiat_movement (record : Records.record) =
  match record.movements with
  | [ { quantity; commodity = Fiat currency; _ } ] -> Some (quantity, currency)
  | _ -> None
;;

let event ~institution (account : Records.account) (record : Records.record) =
  let kind =
    if Option.is_some (fiat_movement record) then movement_kind else operation_kind
  in
  let account_id = account_identity ~institution account in
  let payload = Records.row_to_string account record in
  match record.occurred.value with
  | Instant occurred_at ->
    Source_event.create
      ~venue:institution
      ~account:account_id
      ~kind
      ~source_id:record.id
      ~occurred_at
      ~payload
      ()
  | Date occurred_on ->
    Source_event.create_dated
      ~venue:institution
      ~account:account_id
      ~kind
      ~source_id:record.id
      ~occurred_on
      ~payload
      ()
;;

(* Only what the database can hold as a statement today. *)
let statement ~institution (t : Records.t) ~events (s : Records.statement)
  : Document.statement option
  =
  let balance balance_type =
    List.find t.balances ~f:(fun (b : Records.balance) ->
      String.equal b.account s.account && Poly.equal b.balance_type balance_type)
  in
  let account =
    List.find t.accounts ~f:(fun (a : Records.account) -> String.equal a.id s.account)
  in
  match account, s.period, balance Opening, balance Closing with
  | Some account, Some (first, last), Some opening, Some closing ->
    (match opening.commodity with
     | Fiat currency ->
       Some
         { source_account = account_identity ~institution account
         ; reference = Some s.id
         ; currency
         ; period_start = first.value
         ; period_end = last.value
         ; opening_as_of = opening.as_of.value
         ; closing_as_of = closing.as_of.value
         ; opening = opening.quantity
         ; closing = closing.quantity
         ; events
         }
     | Onchain_native _ | Onchain_token _ | Reported _ -> None)
  | _ -> None
;;

let document ~institution (t : Records.t) =
  let open Or_error.Let_syntax in
  let%bind () =
    if List.exists t.records ~f:(fun (r : Records.record) -> r.observation)
    then Or_error.error_string "importing observations is not supported yet"
    else Ok ()
  in
  let%map rows =
    List.map t.records ~f:(fun (record : Records.record) ->
      match
        List.find t.accounts ~f:(fun (a : Records.account) ->
          String.equal a.id record.account)
      with
      | None -> Or_error.errorf "record %s names an unknown account" record.id
      | Some account ->
        let%map event = event ~institution account record in
        record, event)
    |> Or_error.all
  in
  (* A row listed by several statements is stored once, under the first. *)
  let first_statement (record : Records.record) = List.hd record.statements in
  let listed_by id =
    List.filter_map rows ~f:(fun (record, event) ->
      Option.some_if (Option.equal String.equal (first_statement record) (Some id)) event)
  in
  let stored =
    List.filter_map t.statements ~f:(fun (s : Records.statement) ->
      Option.map
        (statement ~institution t ~events:(listed_by s.id) s)
        ~f:(fun document -> s.id, document))
  in
  { Document.empty with
    statements = List.map stored ~f:snd
  ; events =
      List.filter_map rows ~f:(fun (record, event) ->
        let stored_under_statement =
          Option.exists
            (first_statement record)
            ~f:(List.Assoc.mem stored ~equal:String.equal)
        in
        Option.some_if (not stored_under_statement) event)
  }
;;

let decode (event : Source_event.t) =
  let open Or_error.Let_syntax in
  let%bind (account : Records.account), (record : Records.record) =
    Records.row_of_string event.payload
  in
  let%bind amount, currency =
    Or_error.of_option
      (fiat_movement record)
      ~error:(Error.of_string "this record is an operation, not a bank movement")
  in
  let iban text = Iban.of_string text |> Result.ok in
  let own_iban : Movement.own_iban =
    match account.iban with
    | Iban text ->
      Option.value_map (iban text) ~default:Movement.Unknown ~f:(fun i ->
        Movement.Known i)
    | No_iban -> Not_applicable
    | Unstated -> Unknown
  in
  let details = record.details in
  Movement.create
    ~account:
      { bank_id = event.venue
      ; account_id = account.id
      ; account_type = account_type account
      ; currency
      }
    ~own_iban
    ~booked_on:(Records.local_date record.occurred)
    ?value_on:record.value_on
    ~amount
    ?fee:record.fee
    ?counterparty_iban:
      (Option.bind details.counterparty ~f:(fun c -> Option.bind c.iban ~f:iban))
    ?movement_type:details.movement_type
    ~fields:details.fields
    (match details.descriptions with
     | [] -> [ record.id ]
     | descriptions -> descriptions)
;;

let operation_rows (event : Source_event.t) =
  let open Or_error.Let_syntax in
  let%bind _, (record : Records.record) = Records.row_of_string event.payload in
  let%bind legs =
    List.map record.movements ~f:(fun (m : Records.movement) ->
      Operation_import.leg ~role:m.role ~commodity:m.commodity ~quantity:m.quantity)
    |> Or_error.all
  in
  let%map row = Operation_import.row ~source:event ~legs in
  [ row ]
;;

(* An institution whose files reach fin through a CSV definition or as records files. *)
let make (definition : Csv_converter.t) : (module Adapter.S) =
  let institution = Csv_converter.institution definition in
  let records = "records" in
  let csv = "csv" in
  let module Adapter_impl = struct
    let institution = institution
    let aliases = []
    let kinds = [ movement_kind ]
    let operation_kinds = [ operation_kind ]
    let decode = decode
    let operation_rows = operation_rows
    let operation_group _ = None
    let formats = [ records; csv ]
    let takes_account_label = false

    let probe ~filename:_ contents =
      if Records.is_records_of ~institution contents
      then Some records
      else if Csv_converter.recognizes definition contents
      then Some csv
      else None
    ;;

    let parse ~context:_ ~format contents =
      let open Or_error.Let_syntax in
      let%bind sections =
        if String.equal format records
        then Records.of_string contents
        else (
          let%bind t = Csv_converter.convert definition ~name:"import" contents in
          Records.merge_all [ t ])
      in
      let%map documents = List.map sections ~f:(document ~institution) |> Or_error.all in
      List.fold documents ~init:Document.empty ~f:(fun all (d : Document.t) ->
        { all with
          statements = all.statements @ d.statements
        ; events = all.events @ d.events
        })
    ;;

    let overlap = Csv_converter.overlap definition
    let companions = []
  end
  in
  (module Adapter_impl : Adapter.S)
;;
