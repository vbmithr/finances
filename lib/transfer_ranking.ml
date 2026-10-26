open Core

type edge =
  { first : int64
  ; second : int64
  ; weight : int
  }

type assignment =
  { edge : edge
  ; component : int
  ; best_score : int option
  ; exclusion_margin : int option
  ; inclusion_penalty : int option
  ; status : string
  }

type evidence =
  { code : string
  ; points : int
  ; detail : string
  }

type ranked =
  { candidate : Store.transfer_candidate
  ; score : int
  ; evidence : evidence list
  ; assignment : assignment
  ; confidence : string
  }

let policy = "transfer-evidence-v1"
let max_component_vertices = 18

let assignments edges =
  let open Or_error.Let_syntax in
  let%bind () =
    if List.exists edges ~f:(fun e -> Int64.equal e.first e.second || e.weight <= 0)
    then
      Or_error.error_string "ranking edges require distinct vertices and positive weights"
    else Ok ()
  in
  let key e = Int64.min e.first e.second, Int64.max e.first e.second in
  let%bind () =
    if
      List.contains_dup
        (List.map edges ~f:key)
        ~compare:(Tuple2.compare ~cmp1:Int64.compare ~cmp2:Int64.compare)
    then Or_error.error_string "duplicate ranking edge"
    else Ok ()
  in
  let neighbors = Int64.Table.create () in
  List.iter edges ~f:(fun e ->
    Hashtbl.add_multi neighbors ~key:e.first ~data:e.second;
    Hashtbl.add_multi neighbors ~key:e.second ~data:e.first);
  let seen = Int64.Hash_set.create () in
  let rec flood acc = function
    | [] -> acc
    | id :: rest when Hash_set.mem seen id -> flood acc rest
    | id :: rest ->
      Hash_set.add seen id;
      flood (id :: acc) (Hashtbl.find_exn neighbors id @ rest)
  in
  let components =
    Hashtbl.keys neighbors
    |> List.sort ~compare:Int64.compare
    |> List.filter_map ~f:(fun id ->
      if Hash_set.mem seen id
      then None
      else Some (flood [] [ id ] |> List.sort ~compare:Int64.compare))
  in
  Ok
    (List.concat_mapi components ~f:(fun component vertices ->
       let component = component + 1 in
       let members = Int64.Set.of_list vertices in
       let es = List.filter edges ~f:(fun e -> Set.mem members e.first) in
       if List.length vertices > max_component_vertices
       then
         List.map es ~f:(fun edge ->
           { edge
           ; component
           ; best_score = None
           ; exclusion_margin = None
           ; inclusion_penalty = None
           ; status = "not_evaluated"
           })
       else (
         let positions = Int64.Table.create () in
         List.iteri vertices ~f:(fun pos id -> Hashtbl.set positions ~key:id ~data:pos);
         let indexed =
           List.mapi es ~f:(fun i e ->
             ( i
             , Hashtbl.find_exn positions e.first
             , Hashtbl.find_exn positions e.second
             , e.weight ))
         in
         let adjacent = Array.create ~len:(List.length vertices) [] in
         List.iter indexed ~f:(fun (i, a, b, w) ->
           adjacent.(a) <- (i, b, w) :: adjacent.(a);
           adjacent.(b) <- (i, a, w) :: adjacent.(b));
         let solver banned =
           let memo = Int.Table.create () in
           let rec solve mask =
             if mask = 0
             then 0
             else
               Hashtbl.find_or_add memo mask ~default:(fun () ->
                 let rec first i =
                   if Int.bit_and mask (1 lsl i) <> 0 then i else first (i + 1)
                 in
                 let v = first 0 in
                 let without = Int.bit_xor mask (1 lsl v) in
                 List.fold
                   adjacent.(v)
                   ~init:(solve without)
                   ~f:(fun best (edge, other, weight) ->
                     if
                       Option.equal Int.equal banned (Some edge)
                       || Int.bit_and without (1 lsl other) = 0
                     then best
                     else Int.max best (weight + solve (Int.bit_xor without (1 lsl other)))))
           in
           solve
         in
         let full = (1 lsl List.length vertices) - 1 in
         let solve = solver None in
         let best = solve full in
         List.map2_exn es indexed ~f:(fun edge (i, a, b, weight) ->
           let with_edge =
             weight + solve (Int.bit_xor (Int.bit_xor full (1 lsl a)) (1 lsl b))
           in
           let without_edge = (solver (Some i)) full in
           let status =
             if with_edge < best
             then "not_preferred"
             else if without_edge < best
             then "supported"
             else "alternative"
           in
           { edge
           ; component
           ; best_score = Some best
           ; exclusion_margin = Some (best - without_edge)
           ; inclusion_penalty = Some (best - with_edge)
           ; status
           }))))
;;

let description (tx : Bank_transaction.t) =
  String.concat ~sep:" " (List.filter_opt [ tx.name; tx.memo ])
;;

let bank_mentioned text bank =
  let aliases =
    match bank with
    | "boursobank" -> [ "boursobank"; "boursorama" ]
    | other -> [ other ]
  in
  String.lowercase text
  |> String.split_on_chars ~on:[ ' '; '\t'; '\n'; ':'; ';'; ','; '.'; '/'; '-'; '('; ')' ]
  |> List.exists ~f:(fun word -> List.mem aliases word ~equal:String.equal)
;;

let references text =
  let pattern =
    Re.Perl.compile_pat
      "(?:^|[[:space:]])(?:ref(?:erence)?|end-to-end|endtoend)[[:space:]]*:[[:space:]]*([a-z0-9_-]{6,})"
      ~opts:[ `Caseless ]
  in
  Re.all pattern text
  |> List.filter_map ~f:(fun group ->
    let token = Re.Group.get group 1 |> String.uppercase in
    if
      String.exists token ~f:Char.is_digit
      && not (List.mem [ "NOTPROVIDED"; "NONREF" ] token ~equal:String.equal)
    then Some token
    else None)
;;

let evidence (c : Store.transfer_candidate) =
  let base =
    { code = "opposite_principal"
    ; points = 30
    ; detail = "exact opposite principal amounts in the same currency"
    }
  in
  let timing =
    { code = "booking_dates"
    ; points =
        (if c.days_apart = 0
         then 35
         else if c.days_apart = 1
         then 20
         else Int.max 0 (16 - (3 * c.days_apart)))
    ; detail =
        (if c.days_apart = 0
         then "same booking day"
         else sprintf "booking dates are %d calendar days apart" c.days_apart)
    }
  in
  let extra =
    List.filter_opt
      [ (if c.iban_match
         then
           Some
             { code = "counterparty_iban"
             ; points = 80
             ; detail = "structured counterparty IBAN matches the other account"
             }
         else None)
      ; (if c.unique
         then
           Some
             { code = "no_competing_pair"
             ; points = 10
             ; detail = "no other compatible candidate for either movement"
             }
         else None)
      ; (if
           bank_mentioned (description c.first_transaction) c.second.venue
           || bank_mentioned (description c.second_transaction) c.first.venue
         then
           Some
             { code = "bank_name"
             ; points = 8
             ; detail = "description explicitly names the other bank"
             }
         else None)
      ; (if
           List.exists
             (references (description c.first_transaction))
             ~f:(fun token ->
               List.mem
                 (references (description c.second_transaction))
                 token
                 ~equal:String.equal)
         then
           Some
             { code = "shared_reference"
             ; points = 50
             ; detail = "shared explicit reference token"
             }
         else None)
      ]
  in
  base :: timing :: extra
;;

let analyze candidates =
  let eligible =
    List.filter candidates ~f:(fun c -> c.Store.iban_compatible && Option.is_none c.state)
  in
  let scored =
    List.map eligible ~f:(fun candidate ->
      let evidence = evidence candidate in
      candidate, evidence, List.sum (module Int) evidence ~f:(fun e -> e.points))
  in
  let edges =
    List.map scored ~f:(fun (c, _, weight) ->
      { first = c.Store.first_event_id; second = c.second_event_id; weight })
  in
  let open Or_error.Let_syntax in
  let%map assignments = assignments edges in
  List.map scored ~f:(fun (candidate, evidence, score) ->
    let assignment =
      List.find_exn assignments ~f:(fun a ->
        Int64.equal a.edge.first candidate.first_event_id
        && Int64.equal a.edge.second candidate.second_event_id)
    in
    let confidence =
      if not (String.equal assignment.status "supported")
      then "ambiguous"
      else (
        let margin = Option.value assignment.exclusion_margin ~default:0 in
        if
          score >= 65
          && margin >= 15
          && (candidate.iban_match
              || candidate.days_apart = 0
              || List.exists evidence ~f:(fun e -> String.equal e.code "shared_reference")
             )
        then "high"
        else "medium")
    in
    { candidate; score; evidence; assignment; confidence })
;;
