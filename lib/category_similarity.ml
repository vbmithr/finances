open Base

type t =
  { seed : Store.bank_review
  ; candidates : Store.bank_review list
  ; conflicts : Store.bank_review list
  }

let normalize text =
  String.split_on_chars text ~on:[ ' '; '\t'; '\n'; '\r' ]
  |> List.filter ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:" "
  |> String.lowercase
;;

let key (r : Store.bank_review) =
  match r.movement_type, r.display_name with
  | Some Card, Some name when not (String.is_empty (normalize name)) ->
    `Card_merchant, [ normalize name ]
  | _ -> `Description, List.map r.transaction.descriptions ~f:normalize
;;

let category name =
  String.is_prefix name ~prefix:"expenses:" || String.is_prefix name ~prefix:"income:"
;;

let find ~seed rows =
  let open Or_error.Let_syntax in
  let%bind selected =
    List.find rows ~f:(fun (r : Store.bank_review) -> Int64.equal r.event_id seed)
    |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
  in
  let kind, values = key selected in
  if (not (category selected.counter_account)) || Option.is_none selected.reason
  then
    Or_error.error_string
      "classify the example transaction before finding similar movements"
  else if List.for_all values ~f:String.is_empty
  then Or_error.error_string "example transaction has no description to match"
  else (
    let matches =
      List.filter rows ~f:(fun (r : Store.bank_review) ->
        (not (Int64.equal r.event_id seed))
        && String.equal r.institution selected.institution
        &&
        let other_kind, other_values = key r in
        Poly.equal kind other_kind && List.equal String.equal values other_values)
    in
    let candidates =
      List.filter matches ~f:(fun r ->
        String.equal r.Store.counter_account "equity:unclassified")
    in
    let conflicts =
      List.filter matches ~f:(fun r ->
        category r.Store.counter_account
        && not (String.equal r.counter_account selected.counter_account))
    in
    Ok { seed = selected; candidates; conflicts })
;;
