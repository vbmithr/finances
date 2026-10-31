open Base
open Stdio
open Finances

let ok = Or_error.ok_exn
let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)

let memory f =
  ok
    (Store.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       ignore
         (ok
            (Test_support.import
               db
               ~account_label:"main"
               ~institution:"revolut"
               ~path:"revolut.csv"
               (In_channel.read_all "fixtures/revolut.csv")));
       f db;
       Ok ()))
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
       ~action:(Type (Movement_type.of_string kind)))
;;

let outcomes (result : Store.type_rule_test) =
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
    let gaps = ok (Store.description_gaps db) in
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
      (List.length (ok (Store.description_gaps ~institution:"wise" db)));
    ok (Store.add_classification_rule db (rule "deposits" "^Initial" "deposit"));
    (* Saving a rule changes nothing until types are refreshed, so the gap list
       reads the effective type, which an automatic rule has not yet changed. *)
    check_int "unchanged until refresh" 3 (List.length (ok (Store.description_gaps db))))
;;

let draft () =
  memory (fun db ->
    let defaults = List.length (ok (Store.classification_rules db)) in
    let t = ok (Store.test_type_rule db (rule "repeat" "^Repeated" "transfer")) in
    check_int "scanned" 4 t.scanned;
    Alcotest.(check (list string)) "newly typed" [ "new"; "new" ] (outcomes t);
    check_int "untyped before" 4 t.untyped_before;
    check_int "untyped after" 2 t.untyped_after;
    (* The test saves nothing. *)
    check_int "no rule saved" defaults (List.length (ok (Store.classification_rules db)));
    ok (Store.add_classification_rule db (rule "first" "^Repeated" "deposit"));
    let later =
      ok (Store.test_type_rule db (rule ~priority:2000 "later" "^Repeated" "transfer"))
    in
    Alcotest.(check (list string))
      "earlier rule decides"
      [ "shadowed:first"; "shadowed:first" ]
      (outcomes later);
    check_int "no change in untyped" later.untyped_before later.untyped_after;
    let earlier =
      ok (Store.test_type_rule db (rule ~priority:10 "earlier" "^Repeated" "transfer"))
    in
    Alcotest.(check (list string))
      "earlier priority wins"
      [ "changed"; "changed" ]
      (outcomes earlier);
    (* A draft with a saved rule's name replaces it, and is compared with it. *)
    let same = ok (Store.test_type_rule db (rule "first" "^Repeated" "deposit")) in
    Alcotest.(check (list string)) "unchanged edit" [ "same"; "same" ] (outcomes same);
    let edit = ok (Store.test_type_rule db (rule "first" "^Repeated" "transfer")) in
    Alcotest.(check (list string)) "edited type" [ "changed"; "changed" ] (outcomes edit);
    (* Rules in review mode do not take part in typing. *)
    ok
      (Store.add_classification_rule
         db
         (rule ~mode:Review "review" "^Initial" "interest"));
    let initial = ok (Store.test_type_rule db (rule "initial" "^Initial" "deposit")) in
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
           ~action:(Category "income:other"))
    in
    Alcotest.(check bool)
      "category rejected"
      true
      (Result.is_error (Store.test_type_rule db category));
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
           ~action:(Type (Movement_type.of_string "transfer")))
    in
    let none = ok (Store.test_type_rule db wise) in
    check_int "nothing in scope" 0 none.scanned)
;;

let override () =
  memory (fun db ->
    let event_id =
      List.find_map_exn
        (ok (Store.bank_review db))
        ~f:(fun r ->
          Option.some_if
            (String.is_prefix (List.hd_exn r.transaction.descriptions) ~prefix:"Initial")
            r.event_id)
    in
    ignore
      (ok
         (Store.set_movement_type_override
            db
            ~event_id
            ~movement_type:(Some (Movement_type.of_string "interest"))));
    let t = ok (Store.test_type_rule db (rule "initial" "^Initial" "deposit")) in
    Alcotest.(check (list string)) "override wins" [ "override" ] (outcomes t);
    check_int "an overridden movement is not untyped" 3 t.untyped_before;
    check_int
      "gap list excludes the override"
      2
      (List.length (ok (Store.description_gaps db))))
;;

let () =
  Alcotest.run
    "typing rules"
    [ ( "typing rules"
      , [ Alcotest.test_case "description keys" `Quick keys
        ; Alcotest.test_case "gaps" `Quick gaps
        ; Alcotest.test_case "draft rules" `Quick draft
        ; Alcotest.test_case "manual override" `Quick override
        ] )
    ]
;;
