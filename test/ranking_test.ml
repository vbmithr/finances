open Base
open Stdio
open Finances
open Finances_store

let ok = Or_error.ok_exn
let check = Alcotest.(check int)
let error r = Alcotest.(check bool) "rejected" true (Result.is_error r)

let edge first second weight =
  { Transfer_ranking.first = Int64.of_int first; second = Int64.of_int second; weight }
;;

let assignments es = ok (Transfer_ranking.assignments es)

let status rows a b =
  (List.find_exn rows ~f:(fun r ->
     Int64.equal r.Transfer_ranking.edge.first (Int64.of_int a)
     && Int64.equal r.edge.second (Int64.of_int b)))
    .status
;;

let global_matching () =
  let rows = assignments [ edge 1 3 70; edge 1 4 68; edge 2 3 68 ] in
  Alcotest.(check string) "greedy edge excluded" "not_preferred" (status rows 1 3);
  Alcotest.(check string) "global first pair" "supported" (status rows 1 4);
  Alcotest.(check string) "global second pair" "supported" (status rows 2 3);
  let best = (List.hd_exn rows).best_score |> Option.value_exn in
  check "best total" 136 best;
  let rows = assignments [ edge 1 3 100; edge 1 4 40; edge 2 3 40 ] in
  Alcotest.(check string) "unmatched vertices allowed" "supported" (status rows 1 3)
;;

let ties () =
  let rows = assignments [ edge 1 3 60; edge 1 4 60; edge 2 3 60; edge 2 4 60 ] in
  List.iter rows ~f:(fun r ->
    Alcotest.(check string)
      "ties stay alternatives"
      "alternative"
      r.Transfer_ranking.status;
    check "no decisive margin" 0 (Option.value_exn r.exclusion_margin));
  let reversed = assignments [ edge 2 4 60; edge 2 3 60; edge 1 4 60; edge 1 3 60 ] in
  List.iter reversed ~f:(fun r ->
    Alcotest.(check string)
      "order never breaks ties"
      "alternative"
      r.Transfer_ranking.status)
;;

let bounds () =
  let rows = assignments (List.init 18 ~f:(fun i -> edge 1 (i + 2) 10)) in
  List.iter rows ~f:(fun r ->
    Alcotest.(check string)
      "large graph abstains"
      "not_evaluated"
      r.Transfer_ranking.status);
  error (Transfer_ranking.assignments [ edge 1 1 5 ]);
  error (Transfer_ranking.assignments [ edge 1 2 5; edge 2 1 6 ]);
  error (Transfer_ranking.assignments [ edge 1 2 (-1) ]);
  check "empty graph" 0 (List.length (assignments []))
;;

let bunq_iban = "FR1420041010050500013M02606"
let counterpart_iban = "BE68539007547034"
let other_iban = "DE89370400440532013000"
let replace s a b = String.substr_replace_all s ~pattern:a ~with_:b

let import db institution path contents =
  ignore
    (ok
       (Import.import_bank_file
          ?account_label:(Option.some_if (String.equal institution "revolut") "main")
          db
          ~institution
          ~path
          ~contents))
;;

let seed db =
  let mt =
    In_channel.read_all "fixtures/bunq.mt940"
    |> fun s ->
    replace (replace s "SYNTHETICBUNQ" bunq_iban) "SYNTHETICOTHER" counterpart_iban
  in
  import db "bunq" "bunq.mt940" mt;
  import db "revolut" "revolut.csv" (In_channel.read_all "fixtures/revolut.csv")
;;

let memory f =
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       seed db;
       f db;
       Ok ()))
;;

let identity iban =
  ok
    (Account_identity.create
       ~institution:"revolut"
       ~bank_id:"revolut"
       ~account_id:"main"
       ~account_type:"current-EUR"
       ~iban:(ok (Iban.of_string iban)))
;;

let unique db =
  List.find_exn (ok (Transfers.transfer_candidates db)) ~f:(fun c -> c.Transfers.unique)
;;

let decision c =
  ok
    (Bank_transfer.create
       ~first_event_id:c.Transfers.first_event_id
       ~second_event_id:c.second_event_id
       ~state:Confirmed
       ~reason:"Checked owned accounts")
;;

let ranked_order () =
  memory (fun db ->
    let candidates = ok (Transfers.transfer_candidates db) in
    let rank rows = ok (Transfer_ranking.analyze Builtin.registry rows) in
    let ordered = rank candidates in
    let key (r : Transfer_ranking.ranked) =
      -r.score, r.candidate.first_event_id, r.candidate.second_event_id
    in
    let keys = List.map ordered ~f:key in
    Alcotest.(check bool)
      "descending score and ascending IDs"
      true
      (List.is_sorted keys ~compare:Poly.compare);
    Alcotest.(check bool)
      "input order does not affect ranking"
      true
      (Poly.equal keys (List.map (rank (List.rev candidates)) ~f:key)))
;;

let iban_validation () =
  Alcotest.(check string)
    "normalize spaces/case"
    counterpart_iban
    (Iban.to_string (ok (Iban.of_string "be68 5390 0754 7034")));
  error (Iban.of_string "BE69539007547034");
  error (Iban.of_string "main");
  check
    "null template has no update"
    0
    (List.length
       (ok
          (Account_identity.of_json
             "[{\"institution\":\"revolut\",\"bank_id\":\"revolut\",\"account_id\":\"main\",\"account_type\":\"current-EUR\",\"iban\":null}]")))
;;

let registration () =
  memory (fun db ->
    let c = unique db in
    check
      "registered"
      1
      (ok (Accounts.set_bank_account_identities db [ identity counterpart_iban ]));
    check
      "idempotent"
      0
      (ok (Accounts.set_bank_account_identities db [ identity counterpart_iban ]));
    let info =
      List.find_exn
        (ok (Transfers.bank_account_info db))
        ~f:(fun i -> String.equal i.Transfers.mapping.institution "revolut")
    in
    Alcotest.(check string) "manual provenance" "registered" info.iban_source;
    Alcotest.(check string)
      "saved IBAN"
      counterpart_iban
      (Iban.to_string (Option.value_exn info.iban));
    let ranked =
      ok
        (Transfer_ranking.analyze
           Builtin.registry
           (ok (Transfers.transfer_candidates db)))
    in
    let r =
      List.find_exn ranked ~f:(fun r ->
        Int64.equal r.Transfer_ranking.candidate.first_event_id c.first_event_id
        && Int64.equal r.candidate.second_event_id c.second_event_id)
    in
    Alcotest.(check bool) "positive IBAN evidence" true r.candidate.iban_match;
    Alcotest.(check bool)
      "explicit points"
      true
      (List.exists r.evidence ~f:(fun e ->
         String.equal e.Transfer_ranking.code "counterparty_iban"));
    ignore (ok (Transfers.set_bank_transfers db [ decision c ]));
    error (Accounts.set_bank_account_identities db [ identity other_iban ]);
    let info =
      List.find_exn
        (ok (Transfers.bank_account_info db))
        ~f:(fun i -> String.equal i.Transfers.mapping.institution "revolut")
    in
    Alcotest.(check string)
      "identity conflict rolled back"
      counterpart_iban
      (Iban.to_string (Option.value_exn info.iban)))
;;

let known_contradiction () =
  memory (fun db ->
    let c = unique db in
    let wise =
      In_channel.read_all "fixtures/wise.xml"
      |> fun s -> replace s "SYNTHETIC-ACCOUNT" counterpart_iban
    in
    import db "wise" "wise.xml" wise;
    let all = ok (Transfers.transfer_candidates ~include_reviewed:true db) in
    let conflict =
      List.find_exn all ~f:(fun r ->
        Int64.equal r.Transfers.first_event_id c.first_event_id
        && Int64.equal r.second_event_id c.second_event_id)
    in
    Alcotest.(check bool)
      "known Wise identity contradicts Revolut match"
      false
      conflict.iban_compatible;
    check
      "incompatible pair removed"
      4
      (List.length (ok (Transfers.transfer_candidates db)));
    error (Transfers.set_bank_transfers db [ decision c ]);
    check "no invalid decision" 0 (List.length (ok (Restore.bank_transfer_decisions db))))
;;

let shared_iban () =
  memory (fun db ->
    let original = unique db in
    let csv =
      In_channel.read_all "fixtures/revolut.csv" |> fun s -> replace s ",EUR," ",USD,"
    in
    import db "revolut" "usd.csv" csv;
    let sibling =
      ok
        (Account_identity.create
           ~institution:"revolut"
           ~bank_id:"revolut"
           ~account_id:"main"
           ~account_type:"current-USD"
           ~iban:(ok (Iban.of_string counterpart_iban)))
    in
    ignore (ok (Accounts.set_bank_account_identities db [ sibling ]));
    let pair () =
      List.find_exn
        (ok (Transfers.transfer_candidates ~include_reviewed:true db))
        ~f:(fun c ->
          Int64.equal c.Transfers.first_event_id original.first_event_id
          && Int64.equal c.second_event_id original.second_event_id)
    in
    Alcotest.(check bool)
      "USD sibling does not contradict EUR account"
      true
      (pair ()).iban_compatible;
    Alcotest.(check bool)
      "pair remains visible"
      true
      (List.exists
         (ok (Transfers.transfer_candidates db))
         ~f:(fun c ->
           Int64.equal c.Transfers.first_event_id original.first_event_id
           && Int64.equal c.second_event_id original.second_event_id));
    ignore (ok (Accounts.set_bank_account_identities db [ identity other_iban ]));
    Alcotest.(check bool)
      "explicit EUR contradiction still wins"
      false
      (pair ()).iban_compatible);
  memory (fun db ->
    let original = unique db in
    let csv =
      In_channel.read_all "fixtures/revolut.csv" |> fun s -> replace s ",EUR," ",USD,"
    in
    ignore
      (ok
         (Import.import_bank_file
            ~account_label:"other"
            db
            ~institution:"revolut"
            ~path:"other.csv"
            ~contents:csv));
    let other =
      ok
        (Account_identity.create
           ~institution:"revolut"
           ~bank_id:"revolut"
           ~account_id:"other"
           ~account_type:"current-USD"
           ~iban:(ok (Iban.of_string counterpart_iban)))
    in
    ignore (ok (Accounts.set_bank_account_identities db [ other ]));
    let pair =
      List.find_exn
        (ok (Transfers.transfer_candidates ~include_reviewed:true db))
        ~f:(fun c ->
          Int64.equal c.Transfers.first_event_id original.first_event_id
          && Int64.equal c.second_event_id original.second_event_id)
    in
    Alcotest.(check bool) "different label remains a conflict" false pair.iban_compatible)
;;

let explicit_contradiction () =
  memory (fun db ->
    let c = unique db in
    ignore (ok (Accounts.set_bank_account_identities db [ identity other_iban ]));
    check
      "IBAN contradiction outweighs exact amount"
      4
      (List.length (ok (Transfers.transfer_candidates db)));
    error (Transfers.set_bank_transfers db [ decision c ]);
    let imported =
      ok
        (Account_identity.create
           ~institution:"bunq"
           ~bank_id:"bunq"
           ~account_id:bunq_iban
           ~account_type:"current-EUR"
           ~iban:(ok (Iban.of_string other_iban)))
    in
    error (Accounts.set_bank_account_identities db [ imported ]))
;;

let () =
  Alcotest.run
    "Account identities and ranking"
    [ ( "analysis"
      , List.map
          [ "global matching and unmatched vertices", global_matching
          ; "equal optima remain ambiguous", ties
          ; "bounded exact solver", bounds
          ; "ranked candidates ordered by score", ranked_order
          ; "IBAN validation", iban_validation
          ; "identity registration and confirmed transfer guard", registration
          ; "known account contradictions", known_contradiction
          ; "shared IBAN across currency siblings", shared_iban
          ; "explicit identity contradictions", explicit_contradiction
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
