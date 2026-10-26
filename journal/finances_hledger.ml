open Core
open Finances

let header_text text =
  text
  |> String.map ~f:(fun c -> if Char.to_int c < 32 || Char.equal c '\127' then ' ' else c)
  |> fun s -> String.substr_replace_all s ~pattern:";" ~with_:"；"
;;

let tag_text text =
  header_text text
  |> fun s ->
  String.substr_replace_all s ~pattern:"," ~with_:"，"
  |> fun s -> String.substr_replace_all s ~pattern:":" ~with_:"："
;;

let transaction
      metadata
      wise_metadata
      wise_dates
      (event_id, key, (entry : Accounting.entry))
  =
  let postings =
    List.map entry.postings ~f:(fun posting ->
      Hledger.Journal.posting
        posting.account
        ~amount:(Hledger.Amount.create ~commodity:posting.commodity posting.quantity))
  in
  let observations = Option.value (Hashtbl.find metadata event_id) ~default:[] in
  let first field = List.find_map observations ~f:(fun (_, row) -> field row) in
  let date2 =
    Option.first_some
      (first (fun (row : Bank_csv.row) -> row.value_on))
      (Hashtbl.find wise_dates event_id)
  in
  let wise_extra = Option.value (Hashtbl.find wise_metadata event_id) ~default:[] in
  let extra =
    List.filter_opt
      [ Option.map
          (first (fun row -> row.Bank_csv.category))
          ~f:(fun s -> "bank-category: " ^ tag_text s)
      ; Option.map
          (first (fun row -> row.Bank_csv.parent_category))
          ~f:(fun s -> "bank-parent-category: " ^ tag_text s)
      ; Option.map
          (first (fun row -> row.Bank_csv.suggested_description))
          ~f:(fun s -> "bank-suggested-description: " ^ tag_text s)
      ]
  in
  let provenance =
    if List.is_empty observations
    then []
    else
      [ "csv-row-ids: "
        ^ String.concat
            ~sep:" "
            (List.map observations ~f:(fun (id, _) -> Int64.to_string id))
      ]
  in
  let comment : Hledger.Journal.comment =
    { same_line = Some (sprintf "source-event-id: %Ld" event_id)
    ; lines = [ "projection-key: " ^ key ] @ extra @ provenance @ wise_extra
    }
  in
  Hledger.Journal.transaction
    entry.date
    ?date2
    ~code:(sprintf "event-%Ld" event_id)
    ~description:(header_text entry.description)
    ~comment
    postings
;;

let export db =
  let open Or_error.Let_syntax in
  let%bind entries = Store.entries db in
  let%bind observations = Store.bank_csv_observations db in
  let%bind wise = Store.wise_csv_observations db in
  let%map wise_transactions = Store.wise_transactions db in
  let wise_dates = Int64.Table.create () in
  List.iter wise_transactions ~f:(fun (id, tx) ->
    Option.iter tx.Wise_camt.value_on ~f:(fun date ->
      Hashtbl.set wise_dates ~key:id ~data:date));
  let wise_metadata = Int64.Table.create () in
  List.iter wise ~f:(fun (observation : Store.wise_csv_observation) ->
    match observation.matching with
    | Some { state = Matched; event_id = Some id; _ } ->
      let row = observation.row in
      let tags =
        [ Some ("wise-csv-row-id: " ^ Int64.to_string observation.row_id)
        ; Some ("wise-reported-fees: " ^ tag_text (Bigdecimal.to_string_no_sn row.fees))
        ; Option.map row.merchant ~f:(fun s -> "wise-merchant: " ^ tag_text s)
        ; Option.map row.payment_reference ~f:(fun s ->
            "wise-payment-reference: " ^ tag_text s)
        ]
        |> List.filter_opt
      in
      if not (Hashtbl.mem wise_metadata id)
      then Hashtbl.set wise_metadata ~key:id ~data:tags
    | _ -> ());
  let metadata = Int64.Table.create () in
  List.iter observations ~f:(fun observation ->
    match observation.Store.matching with
    | Some { state = Matched; event_id = Some event_id; _ } ->
      Hashtbl.add_multi metadata ~key:event_id ~data:(observation.row_id, observation.row)
    | _ -> ());
  Hashtbl.map_inplace metadata ~f:(fun rows ->
    List.sort rows ~compare:(fun (a, _) (b, _) -> Int64.compare a b));
  let review_rows =
    List.count observations ~f:(fun row ->
      not
        (Option.exists row.Store.matching ~f:(fun result ->
           Poly.equal result.state Bank_csv_match.Matched)))
  in
  let postings = List.concat_map entries ~f:(fun (_, _, entry) -> entry.postings) in
  let accounts =
    List.map postings ~f:(fun p -> p.account)
    |> List.dedup_and_sort ~compare:String.compare
  in
  let precision = String.Table.create () in
  List.iter postings ~f:(fun p ->
    Hashtbl.update precision p.commodity ~f:(fun previous ->
      Int.max
        (Option.value previous ~default:2)
        (Int.max 0 (-Bigdecimal.exponent p.quantity))));
  let declarations =
    [ Hledger.Journal.Comment_line
        "; Generated from SQLite. Regenerate after database changes."
    ; Comment_line
        "; Bank entries describe movements; opening balances and classifications are not \
         inferred."
    ; Directive (Decimal_mark { mark = '.'; comment = Hledger.Journal.no_comment })
    ]
    @ (Hashtbl.to_alist precision
       |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
       |> List.map ~f:(fun (symbol, precision) ->
         Hledger.Journal.Directive
           (Commodity
              { symbol
              ; sample =
                  Some (Hledger.Amount.create ~commodity:symbol ~precision Bigdecimal.one)
              ; comment = Hledger.Journal.no_comment
              ; subdirectives = []
              })))
    @ List.map accounts ~f:(fun name ->
      Hledger.Journal.Directive
        (Account { name; comment = Hledger.Journal.no_comment; subdirectives = [] }))
  in
  let items =
    declarations
    @ (if List.is_empty observations
       then []
       else
         [ Hledger.Journal.Comment_line
             (sprintf
                "; CSV enrichment: matched_events=%d review_rows=%d. Bank categories are \
                 source suggestions."
                (Hashtbl.length metadata)
                review_rows)
         ])
    @ [ Hledger.Journal.Blank ]
    @ List.concat_map entries ~f:(fun entry ->
      [ Hledger.Journal.Transaction (transaction metadata wise_metadata wise_dates entry)
      ; Blank
      ])
  in
  Hledger.Print.journal items
;;
