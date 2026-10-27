open Base

type state = Bank_match.state =
  | Matched
  | Unmatched
  | Ambiguous
  | Conflict

type input =
  { row_id : int64
  ; document_id : int64
  ; institution : string
  ; row : Bank_csv.row
  }

type candidate =
  { event_id : int64
  ; institution : string
  ; account_id : string
  ; booked_on : Date.t
  ; amount : Quantity.t
  ; descriptions : string list
  }

type result = Bank_match.result =
  { row_id : int64
  ; event_id : int64 option
  ; state : state
  ; reason : string
  ; candidates : int64 list
  }

let policy = "bourso-csv-exact-v1"
let state_name = Bank_match.state_name
let state_of_string = Bank_match.state_of_string

let normalized text =
  String.substr_replace_all text ~pattern:"\194\160" ~with_:" "
  |> fun s ->
  String.substr_replace_all s ~pattern:"\226\128\175" ~with_:" "
  |> String.split_on_chars ~on:[ ' '; '\t'; '\r'; '\n' ]
  |> List.filter ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:" "
;;

let amount_key amount =
  if Quantity.is_zero amount
  then "0"
  else (
    let text = Quantity.to_string amount in
    if not (String.contains text '.')
    then text
    else (
      let rec stop i = if Char.equal text.[i] '0' then stop (i - 1) else i in
      let index = stop (String.length text - 1) in
      String.prefix text (if Char.equal text.[index] '.' then index else index + 1)))
;;

let key institution account date amount =
  Sexp.to_string
    (Sexp.List
       (List.map
          [ institution; account; Date.to_string date; amount_key amount ]
          ~f:(fun s -> Sexp.Atom s)))
;;

let match_rows inputs candidates =
  let index = Hashtbl.create (module String) in
  List.iter candidates ~f:(fun (candidate : candidate) ->
    Hashtbl.add_multi
      index
      ~key:
        (key
           candidate.institution
           candidate.account_id
           candidate.booked_on
           candidate.amount)
      ~data:candidate);
  let provisional =
    List.map inputs ~f:(fun (input : input) ->
      let base =
        Option.value
          (Hashtbl.find
             index
             (key
                input.institution
                input.row.account_id
                input.row.booked_on
                input.row.amount))
          ~default:[]
      in
      let exact =
        List.filter base ~f:(fun c ->
          List.exists c.descriptions ~f:(fun s ->
            String.equal (normalized s) (normalized input.row.description)))
      in
      let ids rows =
        List.map rows ~f:(fun (c : candidate) -> c.event_id)
        |> List.dedup_and_sort ~compare:Int64.compare
      in
      let result =
        match exact with
        | [ one ] ->
          { row_id = input.row_id
          ; event_id = Some one.event_id
          ; state = Matched
          ; reason = "unique exact match"
          ; candidates = [ one.event_id ]
          }
        | [] ->
          { row_id = input.row_id
          ; event_id = None
          ; state = Unmatched
          ; reason =
              (if List.is_empty base
               then "no matching OFX account/date/amount"
               else "original description does not match OFX")
          ; candidates = ids base
          }
        | _ ->
          { row_id = input.row_id
          ; event_id = None
          ; state = Ambiguous
          ; reason = "multiple identical OFX movements"
          ; candidates = ids exact
          }
      in
      input, result)
  in
  let claims = Hashtbl.Poly.create () in
  List.iter provisional ~f:(fun (input, result) ->
    Option.iter result.event_id ~f:(fun event ->
      Hashtbl.incr claims (input.document_id, event)));
  let unique =
    List.map provisional ~f:(fun (input, result) ->
      match result.event_id with
      | Some event when Hashtbl.find_exn claims (input.document_id, event) > 1 ->
        ( input
        , { result with
            event_id = None
          ; state = Ambiguous
          ; reason = "multiple CSV rows in this document claim one OFX movement"
          } )
      | _ -> input, result)
  in
  let observations = Hashtbl.create (module Int64) in
  List.iter unique ~f:(fun (input, result) ->
    Option.iter result.event_id ~f:(fun event ->
      Hashtbl.add_multi observations ~key:event ~data:input.row));
  let conflicts = Hashtbl.create (module Int64) in
  Hashtbl.iteri observations ~f:(fun ~key:event ~data:rows ->
    let fields =
      [ ("value date", fun (r : Bank_csv.row) -> Option.map r.value_on ~f:Date.to_string)
      ; ("suggested description", fun r -> r.suggested_description)
      ; ("category", fun r -> r.category)
      ; ("parent category", fun r -> r.parent_category)
      ; ("comment", fun r -> r.comment)
      ]
    in
    let differing =
      List.filter_map fields ~f:(fun (name, field) ->
        let values =
          List.filter_map rows ~f:field
          |> List.map ~f:normalized
          |> List.dedup_and_sort ~compare:String.compare
        in
        if List.length values > 1 then Some name else None)
    in
    if not (List.is_empty differing)
    then
      Hashtbl.set
        conflicts
        ~key:event
        ~data:("conflicting CSV metadata: " ^ String.concat ~sep:", " differing));
  List.map unique ~f:(fun (_, result) ->
    match Option.bind result.event_id ~f:(Hashtbl.find conflicts) with
    | None -> result
    | Some reason -> { result with state = Conflict; reason })
;;
