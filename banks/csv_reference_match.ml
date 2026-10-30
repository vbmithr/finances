open Base

type input =
  { row_id : int64
  ; document_id : int64
  ; reference : string
  ; booked_at : Ptime.t
  ; amount : Quantity.t
  ; currency : Iso4217.t
  ; conflict_values : string list
  }

type candidate =
  { event_id : int64
  ; reference : string
  ; booked_at : Ptime.t
  ; amount : Quantity.t
  ; currency : Iso4217.t
  }

type messages =
  { matched : string
  ; unmatched : string
  ; ambiguous : string
  ; duplicate_claim : string
  ; conflict : string
  }

let match_rows ~messages rows candidates =
  let amount_key x =
    let s = Quantity.to_string x in
    if Quantity.is_zero x
    then "0"
    else if not (String.contains s '.')
    then s
    else (
      let rec stop i = if Char.equal s.[i] '0' then stop (i - 1) else i in
      let i = stop (String.length s - 1) in
      String.prefix s (if Char.equal s.[i] '.' then i else i + 1))
  in
  let key reference local amount currency =
    Sexp.to_string
      (Sexp.List
         (List.map
            [ reference
            ; Timestamp.to_rfc3339 local
            ; amount_key amount
            ; Iso4217.to_alpha_code currency
            ]
            ~f:(fun s -> Sexp.Atom s)))
  in
  let index = Hashtbl.create (module String) in
  List.iter candidates ~f:(fun (tx : candidate) ->
    Hashtbl.add_multi
      index
      ~key:(key tx.reference tx.booked_at tx.amount tx.currency)
      ~data:tx.event_id);
  let initial =
    List.map rows ~f:(fun (input : input) ->
      let row = input in
      let candidates =
        Option.value
          (Hashtbl.find index (key row.reference row.booked_at row.amount row.currency))
          ~default:[]
        |> List.dedup_and_sort ~compare:Int64.compare
      in
      let event_id, state, reason =
        match candidates with
        | [ id ] -> Some id, Bank_match.Matched, messages.matched
        | [] -> None, Unmatched, messages.unmatched
        | _ -> None, Ambiguous, messages.ambiguous
      in
      input, { Bank_match.row_id = input.row_id; event_id; state; reason; candidates })
  in
  let claims = Hashtbl.Poly.create () in
  List.iter initial ~f:(fun (input, result) ->
    Option.iter result.event_id ~f:(fun id -> Hashtbl.incr claims (input.document_id, id)));
  let unique =
    List.map initial ~f:(fun (input, result) ->
      match result.event_id with
      | Some id when Hashtbl.find_exn claims (input.document_id, id) > 1 ->
        ( input
        , { result with
            event_id = None
          ; state = Ambiguous
          ; reason = messages.duplicate_claim
          } )
      | _ -> input, result)
  in
  let groups = Hashtbl.create (module Int64) in
  List.iter unique ~f:(fun (input, result) ->
    Option.iter result.event_id ~f:(fun id ->
      Hashtbl.add_multi groups ~key:id ~data:input.conflict_values));
  let conflict = Hashtbl.create (module Int64) in
  Hashtbl.iteri groups ~f:(fun ~key:id ~data:observations ->
    let signatures =
      List.dedup_and_sort observations ~compare:(List.compare String.compare)
    in
    if List.length signatures > 1 then Hashtbl.set conflict ~key:id ~data:());
  List.map unique ~f:(fun (_, result) ->
    match result.event_id with
    | Some id when Hashtbl.mem conflict id ->
      { result with state = Conflict; reason = messages.conflict }
    | _ -> result)
;;
