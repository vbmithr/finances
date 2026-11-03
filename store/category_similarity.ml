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

let exact_match (a : Store.bank_review) (b : Store.bank_review) =
  String.equal (normalize a.description) (normalize b.description)
;;

(* Trigrams of ASCII-folded, punctuation-separated bytes. Scores are only a ranking
   heuristic; source text is retained for display and decisions. *)
let trigrams text =
  let normalized =
    String.lowercase text
    |> String.map ~f:(fun c ->
      if Char.is_alphanum c || Char.to_int c >= 128 then c else ' ')
    |> String.split_on_chars ~on:[ ' '; '\t'; '\n'; '\r' ]
    |> List.filter ~f:(Fn.non String.is_empty)
    |> String.concat ~sep:" "
  in
  if String.length normalized < 4
  then []
  else
    List.init
      (String.length normalized - 2)
      ~f:(fun i -> String.sub normalized ~pos:i ~len:3)
    |> List.dedup_and_sort ~compare:String.compare
;;

let fuzzy_text (row : Store.bank_review) = row.description

let fuzzy ~(seed : Store.bank_review) rows =
  let seed_grams = trigrams (fuzzy_text seed) in
  if List.is_empty seed_grams
  then []
  else
    List.filter_map rows ~f:(fun (row : Store.bank_review) ->
      if
        Int64.equal row.event_id seed.event_id
        || exact_match seed row
        || not
             (Poly.equal row.classification Store.Unclassified
              || Poly.equal row.classification Store.Category)
      then None
      else (
        let grams = trigrams (fuzzy_text row) in
        let common =
          List.count seed_grams ~f:(fun g -> List.mem grams g ~equal:String.equal)
        in
        let total = List.length seed_grams + List.length grams in
        if total = 0
        then None
        else (
          let score = Float.of_int (2 * common) /. Float.of_int total in
          if Float.(score < 0.35) then None else Some (row, score))))
    |> List.sort
         ~compare:(fun ((a : Store.bank_review), x) ((b : Store.bank_review), y) ->
           match Float.compare y x with
           | 0 -> Int64.compare a.Store.event_id b.Store.event_id
           | n -> n)
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
  let value = normalize selected.description in
  if (not (category selected.counter_account)) || Option.is_none selected.reason
  then
    Or_error.error_string
      "classify the example transaction before finding similar movements"
  else if String.is_empty value
  then Or_error.error_string "example transaction has no description to match"
  else (
    let matches =
      List.filter rows ~f:(fun (r : Store.bank_review) ->
        (not (Int64.equal r.event_id seed)) && exact_match selected r)
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
