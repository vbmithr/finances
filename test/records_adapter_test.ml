open Base
open Finances_store

let ok = Or_error.ok_exn
let eur = {|{"kind":"fiat","code":"EUR"}|}

let row
      ?(account = "main")
      ?(observation = false)
      ?reference
      ?(descriptions = [])
      id
      ~on
      quantity
  =
  let details =
    String.concat
      ~sep:","
      (List.filter_opt
         [ Option.map reference ~f:(Printf.sprintf {|"references":{"servicer":"%s"}|})
         ; (if List.is_empty descriptions
            then None
            else
              Some
                (Printf.sprintf
                   {|"descriptions":[%s]|}
                   (String.concat
                      ~sep:","
                      (List.map descriptions ~f:(Printf.sprintf "%S")))))
         ])
  in
  Printf.sprintf
    {|{"kind":"record","id":"%s","account":"%s","occurred":"%s","movements":[{"role":"primary","quantity":"%s","commodity":%s}]%s,"details":{%s}}|}
    id
    account
    on
    quantity
    eur
    (if observation then {|,"observation":true|} else "")
    details
;;

let document lines =
  String.concat
    ~sep:"\n"
    ([ {|{"kind":"header","format":"fin-records","version":1,"institution":"test","identity":"source"}|}
     ; {|{"kind":"source","name":"f","sha256":"|} ^ String.make 64 '0' ^ {|"}|}
     ; {|{"kind":"account","id":"main","currency":"EUR"}|}
     ; {|{"kind":"account","id":"other","currency":"EUR"}|}
     ]
     @ lines
     @ [ Printf.sprintf
           {|{"kind":"end","records":%d}|}
           (List.count lines ~f:(String.is_substring ~substring:{|"kind":"record"|}))
       ])
  ^ "\n"
;;

let registry =
  lazy
    (ok
       (Registry.create
          [ Records_adapter.make ~institution:"test" ~overlap:Adapter.Source_ids [] ]))
;;

(* Imports each file in turn, then gives each observation's state by its record ID. *)
let imported files check =
  let path =
    Stdlib.Filename.temp_file
      ~temp_dir:(Sys.getenv_exn "TMPDIR")
      "records-adapter-"
      ".sqlite3"
  in
  Exn.protect
    ~finally:(fun () -> Stdlib.Sys.remove path)
    ~f:(fun () ->
      ok
        (Setup.with_open ~registry:(Lazy.force registry) path (fun db ->
           List.iteri files ~f:(fun i contents ->
             let report =
               ok
                 (Import.import_bank_file
                    db
                    ~institution:"test"
                    ~path:(Printf.sprintf "f%d.jsonl" i)
                    ~contents)
             in
             Alcotest.(check (option string))
               "no refusal"
               None
               (match report.outcome with
                | Rejected | Unsupported | Unreadable -> report.message
                | Imported | Unchanged -> None));
           ignore (ok (Ledger.match_companions db) : Ledger.companion_report);
           let states =
             ok (Ledger.companion_observations db)
             |> List.map ~f:(fun (o : Ledger.companion_observation) ->
               let id =
                 match o.row.fields with
                 | [ payload ] -> (snd (ok (Records.row_of_string payload))).id
                 | _ -> Alcotest.fail "one stored record"
               in
               ( id
               , Option.value_map o.matching ~default:"none" ~f:(fun m ->
                   Bank_match.state_name m.state) ))
           in
           check db states;
           Ok ())))
;;

let states = Alcotest.(list (pair string string))
let sorted = List.sort ~compare:[%compare: string * string]

let by_reference () =
  imported
    [ document
        [ row "c1" ~on:"2025-01-02" "-1" ~reference:"R1"
        ; row "c2" ~on:"2025-01-02" "1" ~reference:"R1"
        ; (* Another account: a reference pairs across the institution's accounts. *)
          row
            "o1"
            ~account:"other"
            ~observation:true
            ~on:"2025-01-02"
            "-1"
            ~reference:"R1"
        ; (* A payment and its refund share a reference; the quantity tells them apart. *)
          row "o2" ~account:"other" ~observation:true ~on:"2025-01-02" "1" ~reference:"R1"
        ; row "o3" ~account:"other" ~observation:true ~on:"2025-01-02" "5" ~reference:"R1"
        ]
    ]
    (fun _ found ->
       Alcotest.check
         states
         "states"
         [ "o1", "matched"; "o2", "matched"; "o3", "unmatched" ]
         (sorted found))
;;

let by_account () =
  imported
    [ document
        [ row "c1" ~on:"2025-01-02" "-1"
        ; row "c2" ~account:"other" ~on:"2025-01-02" "-2"
        ; row "o1" ~observation:true ~on:"2025-01-02" "-1"
        ; (* The right date and quantity on another account is no pair. *)
          row "o2" ~observation:true ~on:"2025-01-02" "-2"
        ; row "o3" ~observation:true ~on:"2025-01-03" "-1"
        ]
    ]
    (fun _ found ->
       Alcotest.check
         states
         "states"
         [ "o1", "matched"; "o2", "unmatched"; "o3", "unmatched" ]
         (sorted found))
;;

let ties () =
  imported
    [ document
        [ (* Descriptions settle which is which. *)
          row "a1" ~on:"2025-01-02" "-1" ~descriptions:[ "Shop" ]
        ; row "a2" ~on:"2025-01-02" "-1" ~descriptions:[ "Cafe" ]
        ; row "oa1" ~observation:true ~on:"2025-01-02" "-1" ~descriptions:[ " CAFE " ]
        ; row "oa2" ~observation:true ~on:"2025-01-02" "-1" ~descriptions:[ "shop" ]
        ; (* Rows that cannot be told apart are interchangeable. *)
          row "b1" ~on:"2025-01-03" "-1" ~descriptions:[ "Same" ]
        ; row "b2" ~on:"2025-01-03" "-1" ~descriptions:[ "Same" ]
        ; row "ob1" ~observation:true ~on:"2025-01-03" "-1"
        ; row "ob2" ~observation:true ~on:"2025-01-03" "-1"
        ; (* Different candidates and nothing to choose by stay for review. *)
          row "c1" ~on:"2025-01-04" "-1" ~descriptions:[ "One" ]
        ; row "c2" ~on:"2025-01-04" "-1" ~descriptions:[ "Two" ]
        ; row "oc1" ~observation:true ~on:"2025-01-04" "-1"
        ; (* Without a reference, date and quantity do not pair rows whose texts disagree. *)
          row "d1" ~on:"2025-01-05" "-1" ~descriptions:[ "One" ]
        ; row "od1" ~observation:true ~on:"2025-01-05" "-1" ~descriptions:[ "Other" ]
        ; (* Two identical rows do not collapse onto one candidate. *)
          row "e1" ~on:"2025-01-06" "-1" ~descriptions:[ "Same" ]
        ; row "oe1" ~observation:true ~on:"2025-01-06" "-1" ~descriptions:[ "Same" ]
        ; row "oe2" ~observation:true ~on:"2025-01-06" "-1" ~descriptions:[ "Same" ]
        ]
    ]
    (fun _ found ->
       Alcotest.check
         states
         "states"
         [ "oa1", "matched"
         ; "oa2", "matched"
         ; "ob1", "matched"
         ; "ob2", "matched"
         ; "oc1", "ambiguous"
         ; "od1", "unmatched"
         ; "oe1", "ambiguous"
         ; "oe2", "ambiguous"
         ]
         (sorted found))
;;

let overlapping_exports () =
  let counted = row "c1" ~on:"2025-01-02" "-1" ~reference:"R" in
  let observed text =
    row "o1" ~observation:true ~on:"2025-01-02" "-1" ~reference:"R" ~descriptions:[ text ]
  in
  (* Another export of one row is no competition, and a late one pairs in any import order. *)
  imported
    [ document [ observed "Text" ]
    ; document [ counted ]
    ; document [ observed "Text"; row "unrelated" ~on:"2025-02-01" "-9" ]
    ]
    (fun _ found ->
       Alcotest.check states "states" [ "o1", "matched"; "o1", "matched" ] (sorted found));
  (* Exports may differ in text: only declared columns can conflict. *)
  imported
    [ document [ counted; observed "One" ]; document [ observed "Two" ] ]
    (fun _ found ->
       Alcotest.check
         states
         "text may differ"
         [ "o1", "matched"; "o1", "matched" ]
         (sorted found))
;;

let exports_of_one_row () =
  (* Each export names its statement differently, and the row stays one row. *)
  let export statement =
    document
      [ Printf.sprintf {|{"kind":"statement","id":"%s","account":"main"}|} statement
      ; String.substr_replace_first
          (row "c1" ~on:"2025-01-02" "-1")
          ~pattern:{|"account":"main",|}
          ~with_:(Printf.sprintf {|"account":"main","statements":["%s"],|} statement)
      ]
  in
  imported
    [ export "first"; export "second" ]
    (fun db _ -> Alcotest.(check int) "one event" 1 (List.length (ok (Ledger.events db))))
;;

let balances () =
  let line ~type_ ~account ~on quantity =
    Printf.sprintf
      {|{"kind":"balance","account":"%s","type":"%s","quantity":"%s","commodity":%s,"as_of":"%s"}|}
      account
      type_
      quantity
      eur
      on
  in
  let file =
    document
      [ line ~type_:"opening" ~account:"main" ~on:"2025-01-01" "10"
      ; line ~type_:"day_end" ~account:"other" ~on:"2025-01-03" "7"
      ; line ~type_:"available" ~account:"main" ~on:"2025-01-02" "9.50"
      ]
  in
  imported [ file; file ] (fun db _ ->
    let found =
      ok (Ledger.stated_balances db)
      |> List.map ~f:(fun (b : Ledger.stated_balance) ->
        ( b.account
        , b.balance_type
        , Quantity.to_string b.quantity
        , Temporal.to_string b.as_of ))
    in
    Alcotest.(check (list (pair string (pair string (pair string string)))))
      "oldest first, and once"
      [ "main", ("opening", ("10", "2025-01-01"))
      ; "main", ("available", ("9.5", "2025-01-02"))
      ; "other", ("day_end", ("7", "2025-01-03"))
      ]
      (List.map found ~f:(fun (a, t, q, d) -> a, (t, (q, d))));
    Alcotest.(check int)
      "by institution"
      0
      (List.length (ok (Ledger.stated_balances ~institution:"elsewhere" db))))
;;

let () =
  Alcotest.run
    "records adapter"
    [ ( "pairing"
      , [ Alcotest.test_case "by reference" `Quick by_reference
        ; Alcotest.test_case "by account, date and quantity" `Quick by_account
        ; Alcotest.test_case "ties" `Quick ties
        ; Alcotest.test_case "overlapping exports" `Quick overlapping_exports
        ; Alcotest.test_case "exports of one row" `Quick exports_of_one_row
        ] )
    ; "balances", [ Alcotest.test_case "stored and listed" `Quick balances ]
    ]
;;
