open Base
open Finances
open Finances_store

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
      descriptions
      posting_dates
      (event_id, key, (entry : Accounting.entry))
  =
  let postings =
    List.mapi entry.postings ~f:(fun index posting ->
      let comment =
        match
          Hashtbl.find posting_dates event_id
          |> Option.bind ~f:(fun dates -> List.nth dates index)
        with
        | Some date when not (Date.equal date entry.date) ->
          Hledger.Journal.
            { same_line = Some ("[" ^ Date.to_string date ^ "]"); lines = [] }
        | _ -> Hledger.Journal.no_comment
      in
      Hledger.Journal.posting
        posting.account
        ~comment
        ~amount:(Hledger.Amount.create ~commodity:posting.commodity posting.quantity))
  in
  (* Matched companion rows, in row order: the first value date wins, and each
     tag comes from the first row that provides it. *)
  let observations = Option.value (Hashtbl.find metadata event_id) ~default:[] in
  let date2 =
    if Hashtbl.mem posting_dates event_id
    then None
    else
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
    ~description:
      (header_text
         (if String.equal key Bank_projection.key
          then
            Option.value_map
              (Hashtbl.find descriptions event_id)
              ~default:entry.description
              ~f:(fun text ->
                if String.is_empty (String.strip text)
                then entry.description
                else String.substr_replace_all text ~pattern:"|" ~with_:"｜")
          else entry.description))
    ~comment
    postings
;;

let export db =
  let open Or_error.Let_syntax in
  let%bind entries = Store.entries db in
  let%bind observations = Store.journal_companions db in
  let%bind category_history = Store.bank_category_history db in
  let%bind movements = Store.journal_movements db in
  let%bind transfer_links = Store.operation_transfer_links db in
  let%bind general_transfers = Store.general_transfer_entries db in
  let%bind exchange_entries = Store.operation_exchange_entries db in
  let posting_dates = Hashtbl.create (module Int64) in
  let pairs = Hashtbl.create (module Int64) in
  let by_event = Hashtbl.create (module Int64) in
  List.iter entries ~f:(fun (id, key, entry) ->
    if String.equal key Bank_projection.key then Hashtbl.set by_event ~key:id ~data:entry);
  let%bind () =
    List.filter transfer_links ~f:(fun link ->
      not
        (List.exists general_transfers ~f:(fun t ->
           Int64.equal t.Store.first.event_id link.first_event_id
           && Int64.equal t.second.event_id link.second_event_id)))
    |> List.map ~f:(fun (d : Store.operation_transfer_link) ->
      match
        Hashtbl.find by_event d.first_event_id, Hashtbl.find by_event d.second_event_id
      with
      | None, None -> Ok ()
      | Some first, Some second ->
        let leg (entry : Accounting.entry) =
          match entry.postings with
          | bank :: _clearing :: fees -> Ok (bank :: fees)
          | _ -> Or_error.error_string "transfer projection has missing postings"
        in
        let%bind first_postings = leg first in
        let%bind second_postings = leg second in
        let date =
          if Date.compare first.date second.date <= 0 then first.date else second.date
        in
        let%bind combined =
          Accounting.entry
            ~date
            ~description:first.description
            (first_postings @ second_postings)
        in
        Hashtbl.set
          posting_dates
          ~key:d.first_event_id
          ~data:
            (List.map first_postings ~f:(fun _ -> first.date)
             @ List.map second_postings ~f:(fun _ -> second.date));
        Hashtbl.set pairs ~key:d.first_event_id ~data:(Some combined);
        Hashtbl.set pairs ~key:d.second_event_id ~data:None;
        Ok ()
      | _ ->
        Or_error.error_string
          "confirmed transfer has only one projected movement; run fin journal project")
    |> Or_error.combine_errors_unit
  in
  let%bind synthetic_transfers =
    general_transfers
    |> List.map ~f:(fun (t : Store.general_transfer_preview) ->
      let bank_leg =
        if t.first.bank
        then Some t.first
        else if t.second.bank
        then Some t.second
        else None
      in
      match bank_leg with
      | None -> Ok (Some (t.first.event_id, "operation:transfer", t.entry))
      | Some leg ->
        (match Hashtbl.find by_event leg.event_id with
         | None ->
           Or_error.error_string
             "confirmed transfer bank movement is not projected; run fin journal project"
         | Some original ->
           (match original.postings with
            | [ bank_posting; _counter ]
              when String.equal bank_posting.account leg.account
                   && String.equal bank_posting.commodity leg.symbol
                   && Quantity.equal bank_posting.quantity leg.quantity ->
              let%bind combined =
                Accounting.entry
                  ~date:t.entry.date
                  ~description:original.description
                  t.entry.postings
              in
              Hashtbl.set pairs ~key:leg.event_id ~data:(Some combined);
              Hashtbl.set
                posting_dates
                ~key:leg.event_id
                ~data:[ t.first.date; t.second.date ];
              Ok None
            | _ ->
              Or_error.error_string
                "bank transfer projection differs from reviewed movement")))
    |> Or_error.combine_errors
    |> Result.map ~f:List.filter_opt
  in
  let entries =
    List.filter_map entries ~f:(fun (id, key, entry) ->
      if not (String.equal key Bank_projection.key)
      then Some (id, key, entry)
      else (
        match Hashtbl.find pairs id with
        | None -> Some (id, key, entry)
        | Some None -> None
        | Some (Some combined) -> Some (id, key, combined)))
  in
  let entries =
    List.stable_sort
      (entries @ exchange_entries @ synthetic_transfers)
      ~compare:
        (fun
          (id_a, key_a, (a : Accounting.entry)) (id_b, key_b, (b : Accounting.entry)) ->
        match Date.compare a.date b.date with
        | 0 ->
          (match Int64.compare id_a id_b with
           | 0 -> String.compare key_a key_b
           | n -> n)
        | n -> n)
  in
  let companions =
    List.map
      (Registry.companions (Store.registry db))
      ~f:(fun (owner, c) -> (owner, c.Companion.format), c)
  in
  let%map annotated =
    observations.rows
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
  List.iter movements ~f:(fun (movement : Store.journal_movement) ->
    Option.iter movement.value_on ~f:(fun date ->
      Hashtbl.set secondary_dates ~key:movement.event_id ~data:date));
  let extra_metadata = Hashtbl.create (module Int64) in
  List.iter transfer_links ~f:(fun (decision : Store.operation_transfer_link) ->
    List.iter [ decision.first_event_id; decision.second_event_id ] ~f:(fun event_id ->
      let tags =
        [ Printf.sprintf
            "source-event-ids: %Ld %Ld"
            decision.first_event_id
            decision.second_event_id
        ; Printf.sprintf
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
  let descriptions = Hashtbl.create (module Int64) in
  List.iter movements ~f:(fun (movement : Store.journal_movement) ->
    let annotations =
      Option.value (Hashtbl.find metadata movement.event_id) ~default:[]
      |> List.map ~f:snd
    in
    Hashtbl.set
      descriptions
      ~key:movement.event_id
      ~data:(Store.effective_description movement.description annotations));
  List.iter transfer_links ~f:(fun (d : Store.operation_transfer_link) ->
    if Hashtbl.mem pairs d.first_event_id
    then (
      let combined =
        Option.value (Hashtbl.find metadata d.first_event_id) ~default:[]
        @ Option.value (Hashtbl.find metadata d.second_event_id) ~default:[]
      in
      if not (List.is_empty combined)
      then Hashtbl.set metadata ~key:d.first_event_id ~data:combined));
  Hashtbl.map_inplace metadata ~f:(fun rows ->
    List.sort rows ~compare:(fun (a, _) (b, _) -> Int64.compare a b));
  let review_rows = observations.review in
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
    @ (if observations.total = 0
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
          (transaction
             metadata
             extra_metadata
             secondary_dates
             descriptions
             posting_dates
             entry)
      ; Blank
      ])
  in
  Hledger.Print.journal items
;;
