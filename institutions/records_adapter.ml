open Base

(* Records as source events, so that everything after import works as it does for any adapter.
   A record states one balance effect in a currency, which is a bank movement, or anything else,
   which is an operation: several legs, a reported commodity, or no effect at all. An observation
   is a companion row, paired with a counted record by [match_rows].

   A stated total is stored separately from period balances. *)

let movement_kind = "records-movement"
let operation_kind = "records-operation"

let account_type (account : Records.account) =
  Option.value account.account_type ~default:"current"
;;

(* The ledger names an account as the institution does. A record's account ID can hold more,
   such as Wise's balance currency, which the account type already says. *)
let bank_id ~institution (account : Records.account) =
  Option.value account.bank ~default:institution
;;

let institution_account_id (account : Records.account) =
  match account.iban, account.number with
  | Iban iban, _ -> iban
  | _, Some number -> number
  | _, None -> account.id
;;

(* The same three parts as the movement's account: fin finds one from the other. *)
let account_identity ~institution (account : Records.account) =
  Json.encode
    (Jsont.list Jsont.string)
    [ bank_id ~institution account; institution_account_id account; account_type account ]
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
  (* Overlapping exports list a row under different statements, and it stays the same row. *)
  let payload = Records.row_to_string account { record with statements = [] } in
  match record.occurred.value with
  | Instant occurred_at ->
    Source_event.create
      ~venue:institution
      ~account:account_id
      ~kind
      ~source_id:record.id
      ~occurred_at
      ~payload
      ~fields:record.details.fields
      ()
  | Date occurred_on ->
    Source_event.create_dated
      ~venue:institution
      ~account:account_id
      ~kind
      ~source_id:record.id
      ~occurred_on
      ~payload
      ~fields:record.details.fields
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
     | Asset _ | Reported _ -> None)
  | _ -> None
;;

let document ~institution (t : Records.t) =
  let open Or_error.Let_syntax in
  let account_of (record : Records.record) =
    List.find t.accounts ~f:(fun (a : Records.account) ->
      String.equal a.id record.account)
    |> Or_error.of_option
         ~error:(Error.of_string ("record " ^ record.id ^ " names an unknown account"))
  in
  let observations, counted =
    List.partition_tf t.records ~f:(fun (r : Records.record) -> r.observation)
  in
  let%bind companion_rows =
    List.mapi observations ~f:(fun index record ->
      let%map account = account_of record in
      index + 1, [ Records.row_to_string account record ])
    |> Or_error.all
  in
  let%map rows =
    List.map counted ~f:(fun (record : Records.record) ->
      let%bind account = account_of record in
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
  let totals =
    List.filter_map t.statements ~f:(fun (s : Records.statement) ->
      Option.bind s.total ~f:(fun amount ->
        List.find t.accounts ~f:(fun (a : Records.account) -> String.equal a.id s.account)
        |> Option.map ~f:(fun account ->
          let events =
            List.filter_map rows ~f:(fun (record, event) ->
              Option.some_if (List.mem record.statements s.id ~equal:String.equal) event)
          in
          { Document.source_account = account_identity ~institution account
          ; reference = s.id
          ; amount
          ; complete =
              (not (List.is_empty events))
              && not
                   (List.exists observations ~f:(fun record ->
                      List.mem record.statements s.id ~equal:String.equal))
          ; events
          })))
  in
  { Document.empty with
    statements = List.map stored ~f:snd
  ; totals
  ; companion_rows
  ; balances =
      List.map t.balances ~f:(fun (b : Records.balance) : Document.balance ->
        { account = b.account
        ; balance_type = b.balance_type
        ; commodity = b.commodity
        ; quantity = b.quantity
        ; as_of = b.as_of.value
        })
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
  (* An event whose payload disagrees with its identity never enters the database. *)
  let%bind () =
    if String.equal event.source_id record.id
    then Ok ()
    else Or_error.error_string "a stored row's identity disagrees with its record"
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
      { bank_id = bank_id ~institution:event.venue account
      ; account_id = institution_account_id account
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
    (* The source event carries the fields, and fin adds them to the movement. *)
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

(* Pairing. A reference states the pair across the institution, whose files may name its
   accounts differently; without one, the account, date and quantity have to agree. *)

let quantity_key (record : Records.record) =
  match record.movements with
  | [ m ] -> Some (Quantity.to_string m.quantity ^ "\000" ^ Commodity.display m.commodity)
  | _ -> None
;;

let date_key (record : Records.record) =
  Date.to_string (Records.local_date record.occurred)
;;

let servicer (record : Records.record) =
  Option.bind record.details.references ~f:(fun r -> r.servicer)
;;

let by_reference record reference =
  Option.map (quantity_key record) ~f:(fun quantity ->
    String.concat ~sep:"\000" [ "reference"; reference; quantity; date_key record ])
;;

let by_account (record : Records.record) =
  Option.map (quantity_key record) ~f:(fun quantity ->
    String.concat ~sep:"\000" [ "account"; record.account; quantity; date_key record ])
;;

let texts (record : Records.record) =
  let name = Option.bind record.details.counterparty ~f:(fun c -> c.name) in
  List.filter_opt [ name ] @ record.details.descriptions
  |> List.map ~f:(fun text ->
    String.lowercase text
    |> String.split_on_chars ~on:[ ' '; '\t'; '\n' ]
    |> List.filter ~f:(Fn.non String.is_empty)
    |> String.concat ~sep:" ")
  |> List.filter ~f:(Fn.non String.is_empty)
;;

let shares_text a b = List.exists (texts a) ~f:(List.mem (texts b) ~equal:String.equal)

(* Several rows on one key: a shared description settles a row, and what remains pairs in source
   order only if the candidates cannot be told apart, so that nothing is enriched wrongly. *)
let settle rows candidates =
  let rec by_text settled rows candidates =
    let claiming candidate =
      List.count rows ~f:(fun (_, record) -> shares_text record candidate)
    in
    let progress =
      List.find_map rows ~f:(fun (row, record) ->
        match
          List.filter candidates ~f:(fun (_, candidate) -> shares_text record candidate)
        with
        | [ (id, candidate) ] when claiming candidate = 1 -> Some (row, id)
        | _ -> None)
    in
    match progress with
    | None -> settled, rows, candidates
    | Some (row, id) ->
      by_text
        ((row, id) :: settled)
        (List.filter rows ~f:(fun (other, _) -> Int64.equal other row |> not))
        (List.filter candidates ~f:(fun (other, _) -> Int64.equal other id |> not))
  in
  let settled, rows, candidates = by_text [] rows candidates in
  let interchangeable =
    List.length rows = List.length candidates
    && List.map candidates ~f:(fun (_, c) -> texts c)
       |> List.dedup_and_sort ~compare:(List.compare String.compare)
       |> List.length
       <= 1
  in
  if interchangeable
  then settled @ List.map2_exn rows candidates ~f:(fun (row, _) (id, _) -> row, id), []
  else settled, List.map rows ~f:(fun (row, _) -> row, List.map candidates ~f:fst)
;;

let match_rows ~conflict_columns (rows : Companion.row list) events =
  let open Or_error.Let_syntax in
  let%bind inputs =
    List.map rows ~f:(fun (row : Companion.row) ->
      match row.fields with
      | [ payload ] ->
        let%map _, record = Records.row_of_string payload in
        row, record
      | _ -> Or_error.error_string "an observation is one stored record")
    |> Or_error.all
  in
  let%map candidates =
    List.map events ~f:(fun (id, (event : Source_event.t)) ->
      let%map _, record = Records.row_of_string event.payload in
      id, record)
    |> Or_error.all
  in
  let index = Hashtbl.create (module String) in
  List.iter candidates ~f:(fun ((_, record) as candidate) ->
    List.filter_opt
      [ Option.bind (servicer record) ~f:(by_reference record); by_account record ]
    |> List.iter ~f:(fun key -> Hashtbl.add_multi index ~key ~data:candidate));
  let candidates_of record =
    let on key = Option.value (Option.bind key ~f:(Hashtbl.find index)) ~default:[] in
    let referenced = on (Option.bind (servicer record) ~f:(by_reference record)) in
    (* Oldest first: the index collects in reverse. Without a reference, date and quantity alone
       do not pair rows whose texts disagree. *)
    List.rev
      (if List.is_empty referenced
       then
         List.filter
           (on (by_account record))
           ~f:(fun (_, candidate) ->
             List.is_empty (texts record)
             || List.is_empty (texts candidate)
             || shares_text record candidate)
       else referenced)
  in
  (* Rows compete for candidates only within one document: another export states the same rows. *)
  let groups = Hashtbl.Poly.create () in
  List.iter inputs ~f:(fun (row, record) ->
    let key = row.document_id, List.map (candidates_of record) ~f:fst in
    Hashtbl.add_multi groups ~key ~data:(row, record));
  let matched = Hashtbl.create (module Int64) in
  let undecided = Hashtbl.create (module Int64) in
  Hashtbl.iter groups ~f:(fun group ->
    let group = List.rev group in
    let candidates =
      match group with
      | (_, record) :: _ -> candidates_of record
      | [] -> []
    in
    let settled, left =
      settle
        (List.map group ~f:(fun ((row : Companion.row), record) -> row.row_id, record))
        candidates
    in
    List.iter settled ~f:(fun (row, id) -> Hashtbl.set matched ~key:row ~data:id);
    List.iter left ~f:(fun (row, ids) -> Hashtbl.set undecided ~key:row ~data:ids));
  (* Exports may differ in text or a user's note. A declared column that differs is for review. *)
  let signature (record : Records.record) =
    List.filter record.details.fields ~f:(fun (name, _) ->
      List.mem conflict_columns name ~equal:String.equal)
    |> List.map ~f:(fun (name, value) -> name ^ "\000" ^ value)
    |> String.concat ~sep:"\000"
  in
  let claims = Hashtbl.create (module Int64) in
  List.iter inputs ~f:(fun ((row : Companion.row), record) ->
    Option.iter (Hashtbl.find matched row.row_id) ~f:(fun id ->
      Hashtbl.add_multi claims ~key:id ~data:(signature record)));
  let conflicting id =
    Hashtbl.find claims id
    |> Option.value ~default:[]
    |> List.dedup_and_sort ~compare:String.compare
    |> List.length
    > 1
  in
  List.map inputs ~f:(fun ((row : Companion.row), _) ->
    let result ?event_id state reason candidates =
      { Bank_match.row_id = row.row_id; event_id; state; reason; candidates }
    in
    match Hashtbl.find matched row.row_id, Hashtbl.find undecided row.row_id with
    | Some id, _ when conflicting id ->
      result ~event_id:id Conflict "exports state this row differently" [ id ]
    | Some id, _ -> result ~event_id:id Matched "paired" [ id ]
    | None, Some (_ :: _ as ids) ->
      result Ambiguous "several counted records could be this row" ids
    | None, _ ->
      result Unmatched "no counted record with this reference or date and quantity" [])
;;

let observation (row : Companion.row) =
  match row.fields with
  | [ payload ] -> Records.row_of_string payload |> Or_error.map ~f:snd
  | _ -> Or_error.error_string "an observation is one stored record"
;;

let companion ~tags ~journal_description ~conflict_columns format : Companion.t =
  { format
  ; policy = "records-v1"
  ; display_name =
      Option.value_map journal_description ~default:Companion.No_display_name ~f:(fun f ->
        Stored_field f)
  ; match_rows = match_rows ~conflict_columns
  ; annotate =
      (fun row ->
        let open Or_error.Let_syntax in
        let%map (record : Records.record) = observation row in
        let details = record.details in
        { Companion.value_on = record.value_on
        ; display_name =
            Option.bind journal_description ~f:(fun field ->
              List.Assoc.find details.fields ~equal:String.equal field)
            |> Option.map ~f:String.strip
        ; movement_type = details.movement_type
        ; tags =
            List.filter_map tags ~f:(fun (tag, field) ->
              List.Assoc.find details.fields ~equal:String.equal field
              |> Option.map ~f:(fun value -> tag, value))
        })
  ; describe =
      (fun row ->
        let open Or_error.Let_syntax in
        let%map (record : Records.record) = observation row in
        [ "id", Some record.id
        ; "occurred", Some record.occurred.stated
        ; ( "quantity"
          , Option.map (List.hd record.movements) ~f:(fun m ->
              Quantity.to_string m.quantity) )
        ; "description", List.hd record.details.descriptions
        ; "reference", servicer record
        ])
  ; fields =
      (fun row ->
        let open Or_error.Let_syntax in
        let%map (record : Records.record) = observation row in
        record.details.fields)
  }
;;

type source =
  { format : string
  ; recognizes : filename:string -> string -> bool
  ; convert :
      ?account_label:string
      -> ?path:string
      -> name:string
      -> string
      -> Records.t Or_error.t
  ; tags : (string * string) list
  ; journal_description : string option
  ; conflict_columns : string list
  ; left_out : string -> int
  ; refusal : string -> string option
  }

(* A statement format, with nothing a CSV layout adds. *)
let statement_source format ~recognizes ~convert =
  { format
  ; recognizes
  ; convert = (fun ?account_label:_ ?path:_ ~name contents -> convert ~name contents)
  ; tags = []
  ; journal_description = None
  ; conflict_columns = []
  ; left_out = (fun _ -> 0)
  ; refusal = (fun _ -> None)
  }
;;

let of_definition format definition =
  { format
  ; recognizes =
      (fun ~filename:_ contents -> Csv_converter.recognizes definition contents)
  ; convert =
      (fun ?account_label:_ ?path:_ ~name contents ->
        Csv_converter.convert definition ~name contents)
  ; refusal = Csv_converter.header_difference definition
  ; tags = Csv_converter.journal_tags definition
  ; journal_description = Csv_converter.journal_description definition
  ; conflict_columns = Csv_converter.conflict_columns definition
  ; left_out =
      (fun contents ->
        Csv_converter.filtered_rows definition contents
        |> Result.ok
        |> Option.value ~default:0)
  }
;;

(* An institution whose files reach fin as records, directly or through a converter. *)
(* [stored_formats] are format names rows already in a database were stored under. *)
let make
      ?(aliases = [])
      ?(stored_formats = [])
      ?(tickers = false)
      ?(own_symbols = [])
      ?(takes_account_label = false)
      ~institution
      ~overlap
      sources
  : (module Adapter.S)
  =
  let records = "records" in
  let module Adapter_impl = struct
    let institution = institution
    let aliases = aliases
    let kinds = [ movement_kind ]
    let operation_kinds = [ operation_kind ]
    let decode = decode
    let operation_rows = operation_rows

    let formats =
      (records :: List.map sources ~f:(fun source -> source.format)) @ stored_formats
    ;;

    let takes_account_label = takes_account_label
    let reports_tickers = tickers
    let own_symbols = own_symbols

    let probe ~filename contents =
      if Records.is_records_of ~institution contents
      then Some records
      else
        List.find_map sources ~f:(fun source ->
          Option.some_if (source.recognizes ~filename contents) source.format)
    ;;

    let parse ~(context : Adapter.context) ~format contents =
      let open Or_error.Let_syntax in
      let%bind sections =
        match List.find sources ~f:(fun source -> String.equal source.format format) with
        | None -> Records.of_string contents
        | Some source ->
          let%bind t =
            source.convert
              ?account_label:context.account_label
              ?path:context.path
              ~name:"import"
              contents
          in
          Records.merge_all [ t ]
      in
      let%map documents = List.map sections ~f:(document ~institution) |> Or_error.all in
      List.fold
        documents
        ~init:Document.empty
        ~f:(fun (all : Document.t) (d : Document.t) ->
          { all with
            statements = all.statements @ d.statements
          ; totals = all.totals @ d.totals
          ; balances = all.balances @ d.balances
          ; events = all.events @ d.events
          ; companion_rows =
              all.companion_rows
              @ List.map d.companion_rows ~f:(fun (number, row) ->
                number + List.length all.companion_rows, row)
          })
    ;;

    let overlap = overlap

    let companions =
      let tags = List.concat_map sources ~f:(fun source -> source.tags) in
      let journal_description =
        List.find_map sources ~f:(fun source -> source.journal_description)
      in
      let conflict_columns =
        List.concat_map sources ~f:(fun source -> source.conflict_columns)
      in
      List.map formats ~f:(companion ~tags ~journal_description ~conflict_columns)
    ;;
  end
  in
  (module Adapter_impl : Adapter.S)
;;
