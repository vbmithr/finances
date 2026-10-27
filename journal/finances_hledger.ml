open Base
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
      extra_metadata
      secondary_dates
      (event_id, key, (entry : Accounting.entry))
  =
  let postings =
    List.map entry.postings ~f:(fun posting ->
      Hledger.Journal.posting
        posting.account
        ~amount:(Hledger.Amount.create ~commodity:posting.commodity posting.quantity))
  in
  (* Matched companion rows, in row order: the first value date wins, and each
     tag comes from the first row that provides it. *)
  let observations = Option.value (Hashtbl.find metadata event_id) ~default:[] in
  let date2 =
    Option.first_some
      (List.find_map observations ~f:(fun (_, (a : Companion.annotation)) -> a.value_on))
      (Hashtbl.find secondary_dates event_id)
  in
  let tags =
    List.fold observations ~init:[] ~f:(fun tags (_, (a : Companion.annotation)) ->
      tags
      @ List.filter a.tags ~f:(fun (name, _) ->
        not (List.Assoc.mem tags name ~equal:String.equal)))
    |> List.map ~f:(fun (name, value) -> name ^ ": " ^ tag_text value)
  in
  let provenance =
    if List.is_empty observations
    then []
    else
      [ "companion-row-ids: "
        ^ String.concat
            ~sep:" "
            (List.map observations ~f:(fun (id, _) -> Int64.to_string id))
      ]
  in
  let review = Option.value (Hashtbl.find extra_metadata event_id) ~default:[] in
  let comment : Hledger.Journal.comment =
    { same_line = Some (Printf.sprintf "source-event-id: %Ld" event_id)
    ; lines = [ "projection-key: " ^ key ] @ tags @ provenance @ review
    }
  in
  Hledger.Journal.transaction
    entry.date
    ?date2
    ~code:(Printf.sprintf "event-%Ld" event_id)
    ~description:(header_text entry.description)
    ~comment
    postings
;;

let export db =
  let open Or_error.Let_syntax in
  let%bind entries = Store.entries db in
  let%bind observations = Store.companion_observations db in
  let%bind category_history = Store.bank_category_history db in
  let%bind movements = Store.movements db in
  let%bind transfer_decisions = Store.bank_transfer_decisions db in
  let companions =
    List.map
      (Registry.companions (Store.registry db))
      ~f:(fun (owner, c) -> (owner, c.Companion.format), c)
  in
  let%map annotated =
    List.filter_map observations ~f:(fun (o : Store.companion_observation) ->
      match o.matching with
      | Some { state = Matched; event_id = Some event_id; _ } -> Some (event_id, o.row)
      | _ -> None)
    |> List.map ~f:(fun (event_id, (row : Companion.row)) ->
      let%bind (companion : Companion.t) =
        Or_error.of_option
          (List.Assoc.find
             companions
             (row.institution, row.format)
             ~equal:[%equal: string * string])
          ~error:(Error.of_string "companion row of an unregistered format")
      in
      let%map annotation = companion.annotate row in
      event_id, (row.row_id, annotation))
    |> Or_error.combine_errors
  in
  let secondary_dates = Hashtbl.create (module Int64) in
  List.iter movements ~f:(fun (id, _, movement) ->
    Option.iter movement.Movement.value_on ~f:(fun date ->
      Hashtbl.set secondary_dates ~key:id ~data:date));
  let extra_metadata = Hashtbl.create (module Int64) in
  List.iter transfer_decisions ~f:(fun (decision : Bank_transfer.decision) ->
    if Poly.equal decision.state Confirmed
    then
      List.iter [ decision.first_event_id; decision.second_event_id ] ~f:(fun event_id ->
        let tags =
          [ Printf.sprintf
              "transfer-pair: %Ld %Ld"
              decision.first_event_id
              decision.second_event_id
          ; "transfer-review-reason: " ^ tag_text decision.reason
          ]
        in
        Hashtbl.update extra_metadata event_id ~f:(fun previous ->
          Option.value previous ~default:[] @ tags)));
  let latest_categories = Hashtbl.create (module Int64) in
  List.iter category_history ~f:(fun (decision : Store.category_change) ->
    Hashtbl.set latest_categories ~key:decision.event_id ~data:decision);
  Hashtbl.iteri latest_categories ~f:(fun ~key:event_id ~data:decision ->
    let tags =
      [ "category-decision-id: " ^ Int64.to_string decision.id
      ; "category-reason: " ^ tag_text decision.reason
      ]
    in
    Hashtbl.update extra_metadata event_id ~f:(fun existing ->
      Option.value existing ~default:[] @ tags));
  let metadata = Hashtbl.create (module Int64) in
  List.iter annotated ~f:(fun (event_id, annotation) ->
    Hashtbl.add_multi metadata ~key:event_id ~data:annotation);
  Hashtbl.map_inplace metadata ~f:(fun rows ->
    List.sort rows ~compare:(fun (a, _) (b, _) -> Int64.compare a b));
  let review_rows =
    List.count observations ~f:(fun row ->
      not
        (Option.exists row.Store.matching ~f:(fun result ->
           Poly.equal result.state Bank_match.Matched)))
  in
  let postings = List.concat_map entries ~f:(fun (_, _, entry) -> entry.postings) in
  let accounts =
    List.map postings ~f:(fun p -> p.account)
    |> List.dedup_and_sort ~compare:String.compare
  in
  let precision = Hashtbl.create (module String) in
  List.iter postings ~f:(fun p ->
    Hashtbl.update precision p.commodity ~f:(fun previous ->
      Int.max
        (Option.value previous ~default:2)
        (Int.max 0 (-Quantity.exponent p.quantity))));
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
                  Some (Hledger.Amount.create ~commodity:symbol ~precision Quantity.one)
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
             (Printf.sprintf
                "; Companion enrichment: matched_events=%d review_rows=%d. Bank \
                 categories are source suggestions."
                (Hashtbl.length metadata)
                review_rows)
         ])
    @ [ Hledger.Journal.Blank ]
    @ List.concat_map entries ~f:(fun entry ->
      [ Hledger.Journal.Transaction
          (transaction metadata extra_metadata secondary_dates entry)
      ; Blank
      ])
  in
  Hledger.Print.journal items
;;
