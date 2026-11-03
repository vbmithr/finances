open Base
open Stdio
open Finances
open Finances_store

let ok = Or_error.ok_exn
let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)

let import_revolut db =
  ignore
    (ok
       (Test_support.import
          db
          ~account_label:"main"
          ~institution:"revolut"
          ~path:"revolut.csv"
          (In_channel.read_all "fixtures/revolut.csv")))
;;

(* With the bundled code mapping removed, every Revolut movement is untyped, which
   is what the description-rule tests need. *)
let memory f =
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       List.iter
         (ok (Import.classification_rules db))
         ~f:(fun r ->
           if String.is_prefix r.name ~prefix:"revolut-reported-"
           then ok (Ledger.delete_classification_rule db r.name));
       import_revolut db;
       f db;
       Ok ()))
;;

let types db =
  List.map
    (ok (Review.bank_review db))
    ~f:(fun r -> List.hd_exn r.transaction.descriptions, r.movement_type)
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
;;

let rule ?(priority = 1000) ?(mode = Classification_rule.Automatic) name pattern kind =
  ok
    (Classification_rule.create
       ~mode
       ~account_type:None
       ~priority
       ~name
       ~pattern:(Some pattern)
       ~institution:(Some "revolut")
       ~match_type:None
       ~case_sensitive:false
       ~action:(Type (Movement_type.of_string kind))
       ())
;;

let outcomes (result : Explain.type_rule_test) =
  List.map result.matches ~f:(fun r ->
    match r.outcome with
    | Newly_typed -> "new"
    | Changed -> "changed"
    | Unchanged -> "same"
    | Shadowed name -> "shadowed:" ^ name
    | Overridden -> "override")
  |> List.sort ~compare:String.compare
;;

let keys () =
  let key = Description_key.key in
  check_string "label then merchant" "prlv sepa" (key "PRLV SEPA NETFLIX 1234");
  check_string
    "at most two words"
    "card transaction"
    (key "Card transaction of 12.50 EUR is");
  check_string "stops at a word with a digit" "paiement" (key "PAIEMENT 0512 CARTE");
  check_string "punctuation is kept" "*interets" (key "*INTERETS 2026");
  check_string "leading digit" "" (key "12 something");
  check_string "blank" "" (key "  \t ");
  check_string "runs of whitespace" "a b" (key "A   B\tC");
  let pattern = Description_key.pattern in
  check_string "pattern" "^prlv\\s+sepa(?:\\s|$)" (pattern "prlv sepa");
  check_string "escaped" "^\\*interets(?:\\s|$)" (pattern "*interets");
  check_string "empty key" "" (pattern "");
  (* The suggested pattern must itself be a valid rule expression that matches. *)
  let compiled =
    ok (Transaction_pattern.compile ~case_sensitive:false (pattern "*interets"))
  in
  Alcotest.(check bool)
    "matches its own source"
    true
    (Transaction_pattern.matches
       compiled
       ~display_name:None
       ~descriptions:[ "*INTERETS 2026" ])
;;

let gaps () =
  memory (fun db ->
    let gaps = ok (Explain.description_gaps db) in
    check_int "groups" 3 (List.length gaps);
    let first = List.hd_exn gaps in
    check_string "largest group first" "repeated transfer" first.key;
    check_int "count" 2 first.count;
    check_int "credits" 2 first.credits;
    check_int "debits" 0 first.debits;
    check_string "pattern" "^repeated\\s+transfer(?:\\s|$)" first.pattern;
    check_int
      "other institution"
      0
      (List.length (ok (Explain.description_gaps ~institution:"wise" db)));
    ok (Ledger.add_classification_rule db (rule "deposits" "^Initial" "deposit"));
    (* Saving a rule types the movements it matches at once, so they leave the list. *)
    check_int "typed at once" 2 (List.length (ok (Explain.description_gaps db))))
;;

let draft () =
  memory (fun db ->
    let defaults = List.length (ok (Import.classification_rules db)) in
    let t = ok (Explain.test_type_rule db (rule "repeat" "^Repeated" "transfer")) in
    check_int "scanned" 4 t.scanned;
    Alcotest.(check (list string)) "newly typed" [ "new"; "new" ] (outcomes t);
    check_int "untyped before" 4 t.untyped_before;
    check_int "untyped after" 2 t.untyped_after;
    (* The test saves nothing. *)
    check_int "no rule saved" defaults (List.length (ok (Import.classification_rules db)));
    ok (Ledger.add_classification_rule db (rule "first" "^Repeated" "deposit"));
    let later =
      ok (Explain.test_type_rule db (rule ~priority:2000 "later" "^Repeated" "transfer"))
    in
    Alcotest.(check (list string))
      "earlier rule decides"
      [ "shadowed:first"; "shadowed:first" ]
      (outcomes later);
    check_int "no change in untyped" later.untyped_before later.untyped_after;
    let earlier =
      ok (Explain.test_type_rule db (rule ~priority:10 "earlier" "^Repeated" "transfer"))
    in
    Alcotest.(check (list string))
      "earlier priority wins"
      [ "changed"; "changed" ]
      (outcomes earlier);
    (* A draft with a saved rule's name replaces it, and is compared with it. *)
    let same = ok (Explain.test_type_rule db (rule "first" "^Repeated" "deposit")) in
    Alcotest.(check (list string)) "unchanged edit" [ "same"; "same" ] (outcomes same);
    let edit = ok (Explain.test_type_rule db (rule "first" "^Repeated" "transfer")) in
    Alcotest.(check (list string)) "edited type" [ "changed"; "changed" ] (outcomes edit);
    (* Rules in review mode do not take part in typing. *)
    ok
      (Ledger.add_classification_rule
         db
         (rule ~mode:Review "review" "^Initial" "interest"));
    let initial = ok (Explain.test_type_rule db (rule "initial" "^Initial" "deposit")) in
    Alcotest.(check (list string)) "review rule ignored" [ "new" ] (outcomes initial);
    (* Only type rules can be tested. *)
    let category =
      ok
        (Classification_rule.create
           ~mode:Automatic
           ~account_type:None
           ~priority:1
           ~name:"cat"
           ~pattern:(Some "^Initial")
           ~institution:(Some "revolut")
           ~match_type:None
           ~case_sensitive:false
           ~action:(Category "income:other")
           ())
    in
    Alcotest.(check bool)
      "category rejected"
      true
      (Result.is_error (Explain.test_type_rule db category));
    (* Another institution's movements are out of scope. *)
    let wise =
      ok
        (Classification_rule.create
           ~mode:Automatic
           ~account_type:None
           ~priority:1
           ~name:"wise-only"
           ~pattern:(Some "^Repeated")
           ~institution:(Some "wise")
           ~match_type:None
           ~case_sensitive:false
           ~action:(Type (Movement_type.of_string "transfer"))
           ())
    in
    let none = ok (Explain.test_type_rule db wise) in
    check_int "nothing in scope" 0 none.scanned)
;;

let override () =
  memory (fun db ->
    let event_id =
      List.find_map_exn
        (ok (Review.bank_review db))
        ~f:(fun r ->
          Option.some_if
            (String.is_prefix (List.hd_exn r.transaction.descriptions) ~prefix:"Initial")
            r.event_id)
    in
    ignore
      (ok
         (Restore.set_movement_type_override
            db
            ~event_id
            ~movement_type:(Some (Movement_type.of_string "interest"))));
    let t = ok (Explain.test_type_rule db (rule "initial" "^Initial" "deposit")) in
    Alcotest.(check (list string)) "override wins" [ "override" ] (outcomes t);
    check_int "an overridden movement is not untyped" 3 t.untyped_before;
    check_int
      "gap list excludes the override"
      2
      (List.length (ok (Explain.description_gaps db))))
;;

let reported_code () =
  (* The bundled Revolut rules map the CSV Type column. *)
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       import_revolut db;
       Alcotest.(check (list (pair string (option string))))
         "typed from the Type column"
         [ "Initial deposit", Some "deposit"
         ; "Repeated transfer", Some "transfer"
         ; "Repeated transfer", Some "transfer"
         ; "Transfer to another bank", Some "transfer"
         ]
         (List.map (types db) ~f:(fun (d, t) ->
            d, Option.map t ~f:Movement_type.to_string));
       check_int "no gaps" 0 (List.length (ok (Explain.description_gaps db)));
       (* Editing the column rule changes types only after an explicit refresh. *)
       let fee =
         ok
           (Classification_rule.create
              ~mode:Automatic
              ~account_type:None
              ~priority:3
              ~name:"revolut-reported-transfer"
              ~pattern:(Some "^Transfer$")
              ~institution:(Some "revolut")
              ~match_type:None
              ~case_sensitive:true
              ~action:(Type (Movement_type.of_string "fee"))
              ~field:"Type"
              ())
       in
       (* Types are derived: editing the rule retypes at once, and says why. *)
       ok (Ledger.replace_classification_rule db fee);
       Alcotest.(check (list string))
         "the three transfers are now fees"
         [ "fee"; "fee"; "fee" ]
         (List.filter_map (types db) ~f:(fun (_, t) ->
            match t with
            | Some Movement_type.Fee -> Some "fee"
            | _ -> None));
       let history = ok (Restore.movement_type_history db) in
       check_int "each change is in the type history" 3 (List.length history);
       List.iter history ~f:(fun (h : Restore.movement_type_history_entry) ->
         Alcotest.(check string)
           "reason"
           "rule revolut-reported-transfer changed"
           h.reason);
       (* One list of rules: a description rule that runs earlier wins, a later one
          is shadowed by the column rule. *)
       let to_bank priority =
         ok
           (Classification_rule.create
              ~mode:Automatic
              ~account_type:None
              ~priority
              ~name:"to-bank"
              ~pattern:(Some "^Transfer to")
              ~institution:(Some "revolut")
              ~match_type:None
              ~case_sensitive:false
              ~action:(Type (Movement_type.of_string "withdrawal"))
              ())
       in
       let early = ok (Explain.test_type_rule db (to_bank 1)) in
       Alcotest.(check (list string))
         "an earlier rule wins"
         [ "changed" ]
         (outcomes early);
       (match early.matches with
        | [ r ] ->
          Alcotest.(check (option string))
            "before"
            (Some "fee")
            (Option.map r.before ~f:Movement_type.to_string);
          Alcotest.(check (option string))
            "after"
            (Some "withdrawal")
            (Option.map r.after ~f:Movement_type.to_string)
        | _ -> Alcotest.fail "one match expected");
       let late = ok (Explain.test_type_rule db (to_bank 1000)) in
       Alcotest.(check (list string))
         "a later rule is shadowed"
         [ "shadowed:revolut-reported-transfer" ]
         (outcomes late);
       Ok ()))
;;

(* The summary reads stored rows; it must agree with the full review. *)
let summary_matches_review () =
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       List.iter [ "wise.xml"; "wise.csv" ] ~f:(fun path ->
         ignore
           (ok
              (Test_support.import
                 db
                 ~institution:"wise"
                 ~path
                 (In_channel.read_all ("fixtures/" ^ path)))));
       ignore (ok (Ledger.match_companions ~institution:"wise" db));
       import_revolut db;
       let from_review institution =
         let rows =
           List.filter
             (ok (Review.bank_review db))
             ~f:(fun r -> String.equal r.institution institution)
         in
         ( List.length rows
         , List.map rows ~f:(fun r ->
             Option.value_map r.movement_type ~default:"" ~f:Movement_type.to_string)
           |> List.sort ~compare:String.compare )
       in
       let from_summary institution =
         let s = ok (Explain.institution_summary db institution) in
         ( s.movements
         , List.concat_map s.by_type ~f:(fun (kind, n) ->
             List.init n ~f:(fun _ ->
               Option.value_map kind ~default:"" ~f:Movement_type.to_string))
           |> List.sort ~compare:String.compare )
       in
       let check name =
         let count, types = from_review name in
         let count', types' = from_summary name in
         check_int (name ^ " movements") count count';
         Alcotest.(check (list string)) (name ^ " types") types types'
       in
       check "wise";
       check "revolut";
       Alcotest.(check bool)
         "typed through the companion"
         true
         (List.exists (snd (from_summary "wise")) ~f:(fun t -> not (String.is_empty t)));
       (* A manual override takes precedence everywhere. *)
       let event_id =
         List.find_map_exn
           (ok (Review.bank_review db))
           ~f:(fun r -> Option.some_if (String.equal r.institution "revolut") r.event_id)
       in
       ignore
         (ok
            (Restore.set_movement_type_override
               db
               ~event_id
               ~movement_type:(Some (Movement_type.of_string "bonus"))));
       check "revolut";
       check "wise";
       let none = ok (Explain.institution_summary db "bunq") in
       check_int "no data" 0 none.movements;
       Alcotest.(check (option string)) "no dates" None none.first_on;
       Ok ()))
;;

let explain () =
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       import_revolut db;
       let id_of text =
         List.find_map_exn
           (ok (Review.bank_review db))
           ~f:(fun r ->
             Option.some_if
               (String.equal (List.hd_exn r.transaction.descriptions) text)
               r.event_id)
       in
       let deposit = id_of "Initial deposit" in
       let e = ok (Explain.explain_movement db deposit) in
       Alcotest.(check (option string))
         "effective"
         (Some "deposit")
         (Option.map e.effective ~f:Movement_type.to_string);
       Alcotest.(check bool) "from the rules" true (Poly.equal e.source Rules);
       (match
          List.find e.checks ~f:(fun c ->
            match c.verdict with
            | Decides _ -> true
            | _ -> false)
        with
        | Some decider ->
          Alcotest.(check string)
            "first match decides"
            "revolut-reported-deposit"
            decider.name
        | None -> Alcotest.fail "expected a deciding rule");
       Alcotest.(check bool)
         "earlier rules that do not match are listed"
         true
         (List.exists e.checks ~f:(fun c ->
            String.equal c.name "revolut-reported-card-payment"
            &&
            match c.verdict with
            | Does_not_match _ -> true
            | _ -> false));
       Alcotest.(check bool)
         "the catch-all also matches"
         true
         (List.exists e.checks ~f:(fun c ->
            String.equal c.name "revolut-reported-other"
            && Poly.equal c.verdict Also_matches));
       (* An earlier description rule takes over at once: the stored type follows. *)
       ok
         (Ledger.add_classification_rule
            db
            (rule ~priority:1 "initial-to-bonus" "^Initial" "bonus"));
       let e = ok (Explain.explain_movement db deposit) in
       Alcotest.(check (option string))
         "rules now give"
         (Some "bonus")
         (Option.map e.rules_type ~f:Movement_type.to_string);
       Alcotest.(check (option string))
         "and so does the stored type"
         (Some "bonus")
         (List.find_map
            (ok (Review.bank_review db))
            ~f:(fun r -> Option.some_if (Int64.equal r.event_id deposit) r.movement_type)
          |> Option.join
          |> Option.map ~f:Movement_type.to_string);
       Alcotest.(check bool)
         "description rule decides"
         true
         (List.exists e.checks ~f:(fun c ->
            String.equal c.name "initial-to-bonus"
            && Poly.equal c.verdict (Decides (Some (Movement_type.of_string "bonus")))));
       (* A manual override outranks every rule. *)
       ignore
         (ok
            (Restore.set_movement_type_override
               db
               ~event_id:deposit
               ~movement_type:(Some (Movement_type.of_string "interest"))));
       let e = ok (Explain.explain_movement db deposit) in
       Alcotest.(check bool) "override wins" true (Poly.equal e.source Manual_override);
       Alcotest.(check bool)
         "unknown id"
         true
         (Result.is_error (Explain.explain_movement db 999999L));
       Ok ()))
;;

let explain_companion () =
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       List.iter [ "wise.xml"; "wise.csv" ] ~f:(fun path ->
         ignore
           (ok
              (Test_support.import
                 db
                 ~institution:"wise"
                 ~path
                 (In_channel.read_all ("fixtures/" ^ path)))));
       ignore (ok (Ledger.match_companions ~institution:"wise" db));
       (* The columns of the matched CSV rows are fields: a rule on one of them decides,
          and the explanation shows the value it matched. *)
       let all =
         List.map
           (ok (Review.bank_review db))
           ~f:(fun r -> ok (Explain.explain_movement db r.event_id))
       in
       check_int
         "every movement is typed by a rule"
         3
         (List.count all ~f:(fun e -> Poly.equal e.source Rules));
       List.iter all ~f:(fun e ->
         Alcotest.(check bool)
           "a rule on a companion column decides"
           true
           (List.exists e.checks ~f:(fun c ->
              (match c.verdict with
               | Decides _ -> true
               | _ -> false)
              && (String.is_prefix c.input ~prefix:"Transaction Details Type: "
                  || String.is_prefix c.input ~prefix:"TransferWise ID: "))));
       Ok ()))
;;

(* A rule may name a companion column: the fixture's cashback row has the bank
   code CARD but a BALANCE_CASHBACK- identifier. *)
let field_rule () =
  let id_rule =
    ok
      (Classification_rule.create
         ~mode:Automatic
         ~account_type:None
         ~priority:(-2000)
         ~name:"id-cashback"
         ~pattern:(Some "^BALANCE_CASHBACK-")
         ~institution:(Some "wise")
         ~match_type:None
         ~case_sensitive:true
         ~action:(Type (Movement_type.of_string "bonus"))
         ~field:"TransferWise ID"
         ())
  in
  let import db =
    (* Start from the bank code alone; the shipped ID rules are tested below. *)
    List.iter [ "wise-id-fee"; "wise-id-cashback" ] ~f:(fun name ->
      ok (Ledger.delete_classification_rule db name));
    List.iter [ "wise.xml"; "wise.csv" ] ~f:(fun path ->
      ignore
        (ok
           (Test_support.import
              db
              ~institution:"wise"
              ~path
              (In_channel.read_all ("fixtures/" ^ path)))));
    ignore (ok (Ledger.match_companions ~institution:"wise" db))
  in
  let types db =
    (ok (Explain.institution_summary db "wise")).by_type
    |> List.map ~f:(fun (kind, n) ->
      Option.value_map kind ~default:"" ~f:Movement_type.to_string, n)
    |> List.sort ~compare:Poly.compare
  in
  let pairs = Alcotest.(list (pair string int)) in
  (* A rule saved before the import decides at import. *)
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       ok (Ledger.add_classification_rule db id_rule);
       import db;
       Alcotest.check
         pairs
         "typed once the rows match"
         [ "bonus", 1; "card", 2 ]
         (types db);
       Ok ()));
  (* A rule added afterwards retypes at once, and removing it puts the bank code back. *)
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       import db;
       Alcotest.check pairs "before" [ "card", 3 ] (types db);
       ok (Ledger.add_classification_rule db id_rule);
       Alcotest.check pairs "after" [ "bonus", 1; "card", 2 ] (types db);
       (match ok (Restore.movement_type_history db) with
        | [ h ] ->
          Alcotest.(check string) "why" "rule id-cashback added" h.reason;
          Alcotest.(check bool)
            "from card to bonus"
            true
            (Option.equal
               Poly.equal
               h.previous_type
               (Some (Movement_type.of_string "card"))
             && Option.equal
                  Poly.equal
                  h.movement_type
                  (Some (Movement_type.of_string "bonus")))
        | other -> Alcotest.failf "one history entry expected, got %d" (List.length other));
       ok (Ledger.delete_classification_rule db "id-cashback");
       Alcotest.check pairs "restored" [ "card", 3 ] (types db);
       check_int
         "both changes are recorded"
         2
         (List.length (ok (Restore.movement_type_history db)));
       Ok ()));
  (* A review-mode type rule cannot name a field (it works from stored transactions,
     which keep none), nor can a category rule. An absent column matches nothing. *)
  let with_field ?(mode = Classification_rule.Automatic) action =
    Classification_rule.create
      ~mode
      ~account_type:None
      ~priority:0
      ~name:"bad"
      ~pattern:(Some "x")
      ~institution:(Some "wise")
      ~match_type:None
      ~case_sensitive:false
      ~action
      ~field:"TransferWise ID"
      ()
  in
  Alcotest.(check bool)
    "an automatic type rule may name a field"
    true
    (Result.is_ok (with_field (Type (Movement_type.of_string "card"))));
  Alcotest.(check bool)
    "a review-mode type rule may not"
    true
    (Result.is_error (with_field ~mode:Review (Type (Movement_type.of_string "card"))));
  Alcotest.(check bool)
    "a category rule may not"
    true
    (Result.is_error (with_field (Category "expenses:food")));
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       let missing =
         ok
           (Classification_rule.create
              ~mode:Automatic
              ~account_type:None
              ~priority:(-2000)
              ~name:"missing-column"
              ~pattern:(Some ".+")
              ~institution:(Some "wise")
              ~match_type:None
              ~case_sensitive:true
              ~action:(Type (Movement_type.of_string "bonus"))
              ~field:"No Such Column"
              ())
       in
       ok (Ledger.add_classification_rule db missing);
       import db;
       Alcotest.check pairs "falls back to the code" [ "card", 3 ] (types db);
       Ok ()))
;;

(* The shipped Wise rules type by TransferWise ID, ahead of the bank code. *)
let shipped_wise_rules () =
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       List.iter [ "wise.xml"; "wise.csv" ] ~f:(fun path ->
         ignore
           (ok
              (Test_support.import
                 db
                 ~institution:"wise"
                 ~path
                 (In_channel.read_all ("fixtures/" ^ path)))));
       ignore (ok (Ledger.match_companions ~institution:"wise" db));
       let by_type =
         (ok (Explain.institution_summary db "wise")).by_type
         |> List.map ~f:(fun (kind, n) ->
           Option.value_map kind ~default:"" ~f:Movement_type.to_string, n)
         |> List.sort ~compare:Poly.compare
       in
       Alcotest.(check (list (pair string int)))
         "the cashback ID outranks the CARD code"
         [ "card", 2; "interest", 1 ]
         by_type;
       let rules =
         List.map (ok (Import.classification_rules db)) ~f:(fun r -> r.name, r.field)
       in
       Alcotest.(check (option (option string)))
         "fee rule matches the ID column"
         (Some (Some "TransferWise ID"))
         (List.Assoc.find rules "wise-id-fee" ~equal:String.equal);
       Alcotest.(check (option (option string)))
         "the code rules name their column too"
         (Some (Some "Transaction Details Type"))
         (List.Assoc.find rules "wise-reported-card" ~equal:String.equal);
       Ok ()))
;;

(* [description] is the adapter's main text, [text] is any, and every text and
   column the source carries has its own name. *)
let primary_fields () =
  let draft ?(name = "draft") ?field ~institution pattern =
    ok
      (Classification_rule.create
         ~mode:Automatic
         ~account_type:None
         ~priority:0
         ~name
         ~pattern:(Some pattern)
         ~institution:(Some institution)
         ~match_type:None
         ~case_sensitive:true
         ~action:(Type (Movement_type.of_string "bonus"))
         ?field
         ())
  in
  let matches db ?field ~institution pattern =
    List.length
      (ok (Explain.test_type_rule db (draft ?field ~institution pattern))).matches
  in
  (* Bunq: the counterparty name comes first and the payment text second. *)
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       ignore
         (ok
            (Test_support.import
               db
               ~institution:"bunq"
               ~path:"bunq.mt940"
               (In_channel.read_all "fixtures/bunq.mt940")));
       let count = matches db ~institution:"bunq" in
       Alcotest.(check bool)
         "description is the payment text"
         true
         (count "^Purchase$" > 0);
       check_int "the name is not the description" 0 (count "^Synthetic shop$");
       Alcotest.(check bool)
         "but it is a field"
         true
         (count ~field:"name" "^Synthetic shop$" > 0);
       Alcotest.(check bool)
         "text is any description"
         true
         (count ~field:"text" "^Synthetic shop$" > 0
          && count ~field:"text" "^Purchase$" > 0);
       check_int "a field the source lacks matches nothing" 0 (count ~field:"memo" ".");
       (* The stored movement keeps which text is the main one, so the gap finder
          groups Bunq by the payment text, as the rules do. *)
       let keys =
         List.map
           (ok (Explain.description_gaps ~institution:"bunq" db))
           ~f:(fun g -> g.key)
       in
       Alcotest.(check bool)
         "the payment text is a group"
         true
         (List.mem keys "purchase" ~equal:String.equal);
       Alcotest.(check bool)
         "the counterparty name is not"
         false
         (List.mem keys "synthetic shop" ~equal:String.equal);
       Ok ()));
  (* The columns a payload drops are stored with the event and reloaded with it. *)
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       import_revolut db;
       let events = ok (Ledger.events db) in
       check_int "four events" 4 (List.length events);
       List.iter events ~f:(fun (_, (e : Source_event.t)) ->
         Alcotest.(check (option string))
           "Product"
           (Some "Current")
           (List.Assoc.find e.fields "Product" ~equal:String.equal);
         Alcotest.(check bool)
           "every header column"
           true
           (List.length e.fields = 10
            && List.Assoc.mem e.fields "Balance" ~equal:String.equal));
       let any = (List.hd_exn events |> snd).fields in
       check_int
         "in file order"
         0
         (Option.value_exn
            (List.findi any ~f:(fun _ (n, _) -> String.equal n "Type")
             |> Option.map ~f:fst));
       (* A rule on a dropped column matches through the stored fields. *)
       let t =
         ok
           (Explain.test_type_rule
              db
              (draft ~field:"Product" ~institution:"revolut" "^Current$"))
       in
       check_int "all four match on Product" 4 (List.length t.matches);
       Ok ()));
  (* OFX: name and memo are fields; the main description is the first present. *)
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       ignore
         (ok
            (Test_support.import_ofx
               db
               ~institution:"boursobank"
               (In_channel.read_all "fixtures/bank.ofx")));
       let count = matches db ~institution:"boursobank" in
       Alcotest.(check bool) "name" true (count ~field:"name" "^CAF" > 0);
       Alcotest.(check bool) "memo" true (count ~field:"memo" "^Synthetic precision" > 0);
       check_int
         "a memo is not in the name"
         0
         (count ~field:"name" "^Synthetic precision");
       Ok ()));
  (* CAMT: the bank transaction code is the field reference, without a CSV. *)
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       ignore
         (ok
            (Test_support.import
               db
               ~institution:"wise"
               ~path:"wise.xml"
               (In_channel.read_all "fixtures/wise.xml")));
       let t =
         ok
           (Explain.test_type_rule
              db
              (draft ~field:"reference" ~institution:"wise" "^BALANCE_CASHBACK-"))
       in
       check_int "one cashback by its reference" 1 (List.length t.matches);
       (* A saved rule on a field is applied by a refresh, and explain names the field. *)
       ok
         (Ledger.add_classification_rule
            db
            (draft
               ~name:"ref-cashback"
               ~field:"reference"
               ~institution:"wise"
               "^BALANCE_CASHBACK-"));
       (* The saved rule retypes at once; explain shows the value it matched. *)
       let history = ok (Restore.movement_type_history db) in
       check_int "one movement changes" 1 (List.length history);
       let event_id = (List.hd_exn history).event_id in
       let e = ok (Explain.explain_movement db event_id) in
       Alcotest.(check bool)
         "explain shows the field's value"
         true
         (List.exists e.checks ~f:(fun c ->
            String.equal c.name "ref-cashback"
            && String.is_prefix c.input ~prefix:"reference: BALANCE_CASHBACK-"));
       Ok ()))
;;

let show () =
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       List.iter [ "wise.xml"; "wise.csv" ] ~f:(fun path ->
         ignore
           (ok
              (Test_support.import
                 db
                 ~institution:"wise"
                 ~path
                 (In_channel.read_all ("fixtures/" ^ path)))));
       ignore (ok (Ledger.match_companions ~institution:"wise" db));
       let event_id = (List.hd_exn (ok (Review.bank_review db))).event_id in
       let d = ok (Explain.show_movement db event_id) in
       Alcotest.(check string) "institution" "wise" d.venue;
       Alcotest.(check bool)
         "payload is shown"
         true
         (String.is_substring d.payload ~substring:"wise-camt-v1");
       Alcotest.(check (list string))
         "imported from"
         [ "wise.xml" ]
         (List.concat_map d.documents ~f:(fun doc -> doc.paths));
       (match d.companions with
        | [ c ] ->
          Alcotest.(check string) "format" "wise_csv" c.format;
          Alcotest.(check bool)
            "every column by header"
            true
            (List.mem (List.map c.fields ~f:fst) "TransferWise ID" ~equal:String.equal
             && List.mem (List.map c.fields ~f:fst) "Merchant" ~equal:String.equal);
          Alcotest.(check (list string))
            "the file it came from"
            [ "wise.csv" ]
            c.document.paths;
          ()
        | _ -> Alcotest.fail "expected one matched companion row");
       Alcotest.(check bool)
         "unknown id"
         true
         (Result.is_error (Explain.show_movement db 999999L));
       Ok ()))
;;

let () =
  Alcotest.run
    "typing rules"
    [ ( "typing rules"
      , [ Alcotest.test_case "description keys" `Quick keys
        ; Alcotest.test_case "gaps" `Quick gaps
        ; Alcotest.test_case "draft rules" `Quick draft
        ; Alcotest.test_case "manual override" `Quick override
        ; Alcotest.test_case "primary CSV type column" `Quick reported_code
        ; Alcotest.test_case "institution summary" `Quick summary_matches_review
        ; Alcotest.test_case "explain" `Quick explain
        ; Alcotest.test_case "explain companion code" `Quick explain_companion
        ; Alcotest.test_case "companion column rules" `Quick field_rule
        ; Alcotest.test_case "shipped Wise rules" `Quick shipped_wise_rules
        ; Alcotest.test_case "primary fields" `Quick primary_fields
        ; Alcotest.test_case "transaction detail" `Quick show
        ] )
    ]
;;
