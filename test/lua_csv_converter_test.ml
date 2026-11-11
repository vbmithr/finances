open Base
open Finances_core
open Finances_institutions

let ok = Or_error.ok_exn
let bundled name = List.Assoc.find_exn Bundled_definitions.csv name ~equal:String.equal

let header =
  "Timestamp (UTC),Transaction Description,Currency,Amount,To Currency,To Amount,Native \
   Currency,Native Amount,Native Amount (in USD),Transaction Kind,Transaction Hash\n"
;;

let compare_converters () =
  let json =
    Csv_converter.of_string (Stdio.In_channel.read_all "fixtures/cdc-converter.json")
    |> ok
  in
  let lua = Lua_csv_converter.create ~institution:"cdc" (bundled "lua/cdc.lua") |> ok in
  let kinds =
    [ "referral_card_cashback"
    ; "mco_stake_reward"
    ; "gift_card_reward"
    ; "referral_bonus"
    ; "referral_gift"
    ; "crypto_deposit"
    ; "exchange_to_crypto_transfer"
    ; "reimbursement"
    ; "supercharger_withdrawal"
    ; "lockup_unlock"
    ; "card_top_up"
    ; "card_cashback_reverted"
    ; "supercharger_deposit"
    ; "lockup_lock"
    ; "crypto_viban_exchange"
    ; "crypto_exchange"
    ; "trading.crypto_conversion.cash"
    ; "viban_purchase"
    ; "unknown_kind"
    ]
  in
  let rows =
    List.concat_map kinds ~f:(fun kind ->
      List.concat_map [ "1.1234567890123456789"; "-2"; "0" ] ~f:(fun amount ->
        List.map
          [ "", ""; "EUR", "0.2"; "EUR", "-1" ]
          ~f:(fun (dest, value) ->
            Printf.sprintf
              "2025-06-09 17:56:16,%s-%s-%s,CRO,%s,%s,%s,EUR,0.10,0.12,%s,\n"
              kind
              amount
              value
              amount
              dest
              value
              kind)))
  in
  let contents = header ^ String.concat rows in
  let old = Csv_converter.convert json ~name:"sample.csv" contents |> ok in
  let new_ = Lua_csv_converter.convert lua ~name:"sample.csv" contents |> ok in
  Alcotest.(check string)
    "identical records"
    (Records.to_string old)
    (Records.to_string new_);
  Alcotest.(check bool)
    "recognizes header"
    true
    (Lua_csv_converter.recognizes lua contents);
  Alcotest.(check bool)
    "rejects wrong header"
    false
    (Lua_csv_converter.recognizes lua "wrong,header\n");
  let invalid = header ^ "bad-date,Oops,CRO,1,,,EUR,1,1,crypto_deposit,\n" in
  Alcotest.(check bool)
    "rejects bad date"
    true
    (Result.is_error (Lua_csv_converter.convert lua ~name:"sample.csv" invalid));
  let mixed =
    "version = 1\ninstitution = \"cdc\"\ntype = \"crypto-exchange\"\n"
    ^ "[[statements]]\nformat = \"csv\"\ndefinition = \"old.json\"\n"
    ^ "[[statements]]\nformat = \"csv\"\nscript = \"new.lua\"\n"
  in
  let load = function
    | "old.json" -> Ok (Stdio.In_channel.read_all "fixtures/cdc-converter.json")
    | "new.lua" -> Ok (bundled "lua/cdc.lua")
    | _ -> Or_error.error_string "unexpected path"
  in
  Alcotest.(check bool)
    "rejects mixing JSON and Lua layouts"
    true
    (Result.is_error (Institution_toml.decode mixed ~load));
  Alcotest.(check bool)
    "rejects institution mismatch"
    true
    (Result.is_error
       (Lua_csv_converter.create ~institution:"other" (bundled "lua/cdc.lua")))
;;

let fdm_account_naming () =
  let lua = Lua_csv_converter.create ~institution:"fdm" (bundled "lua/fdm.lua") |> ok in
  let invalid_link =
    String.substr_replace_first
      (bundled "lua/fdm.lua")
      ~pattern:"link_statements = yes"
      ~with_:"link_statements = \"bad\""
  in
  Alcotest.(check bool)
    "rejects invalid statement-link option"
    true
    (Result.is_error (Lua_csv_converter.create ~institution:"fdm" invalid_link));
  let contents =
    "Type,Merchant,Category,Date,Local currency,Local amount,Exchange rate,USD,Balance \
     after,Note\n\
     Purchase,Shop,Shopping,2025-01-02,EUR,10.00,1.1,11.00,,\n"
  in
  let account ?account_label ?path () =
    let records =
      Lua_csv_converter.convert_with ?account_label ?path lua ~name:"import" contents
      |> ok
      |> Records.to_string
    in
    List.find_map [ "1118"; "2233"; "card" ] ~f:(fun id ->
      Option.some_if
        (String.is_substring records ~substring:(Printf.sprintf "\"id\":\"%s\"" id))
        id)
  in
  let check name expected actual =
    Alcotest.(check (option string)) name (Some expected) actual
  in
  check "card without a file" "card" (account ());
  check "named by its file" "1118" (account ~path:"statements/fdm/1118.csv" ());
  check
    "an explicit label wins"
    "2233"
    (account ~account_label:"2233" ~path:"statements/fdm/1118.csv" ())
;;

let bunq_equivalence () =
  let old =
    Csv_converter.of_string (Stdio.In_channel.read_all "fixtures/bunq-converter.json")
    |> ok
  in
  let lua = Lua_csv_converter.create ~institution:"bunq" (bundled "lua/bunq.lua") |> ok in
  Alcotest.(check (list (pair string string)))
    "journal tag mapping"
    (Csv_converter.journal_tags old)
    (Lua_csv_converter.tags lua);
  let invalid_tags =
    String.substr_replace_first
      (bundled "lua/bunq.lua")
      ~pattern:"= \"Counterparty\""
      ~with_:"= \"Missing\""
  in
  Alcotest.(check bool)
    "reject unknown journal column"
    true
    (Result.is_error (Lua_csv_converter.create ~institution:"bunq" invalid_tags));
  let compare name contents =
    let expected = Csv_converter.convert old ~name contents |> ok in
    let actual = Lua_csv_converter.convert lua ~name contents |> ok in
    Alcotest.(check string) name (Records.to_string expected) (Records.to_string actual)
  in
  compare "fixture.csv" (Stdio.In_channel.read_all "fixtures/bunq.csv");
  let header = "Date,Interest Date,Amount,Account,Counterparty,Name,Description\n" in
  compare
    "many.csv"
    (header
     ^ "2025-01-01,2025-01-02,\"1 234,50\",one,BE68539007547034, Shop ,Text\n\
        2025-01-01,2025-01-02,\"-1,00\",two,invalid,,Text\n\
        2025-01-01,2025-01-02,\"-1,00\",two,invalid,,Text\n");
  compare "empty.csv" header;
  let contradictory field value =
    let source =
      String.substr_replace_first
        (bundled "lua/bunq.lua")
        ~pattern:"kind = \"record\","
        ~with_:(Printf.sprintf "kind = \"record\", %s = %s," field value)
    in
    let converter = Lua_csv_converter.create ~institution:"bunq" source |> ok in
    let row = header ^ "2025-01-01,2025-01-02,\"1,00\",one,,,Text\n" in
    Alcotest.(check bool)
      ("reject contradictory " ^ field)
      true
      (Result.is_error (Lua_csv_converter.convert converter ~name:"bad.csv" row))
  in
  contradictory "account" "\"other\"";
  contradictory "observation" "no";
  let conflicting_source =
    String.substr_replace_first
      (bundled "lua/bunq.lua")
      ~pattern:"bank = \"bunq\""
      ~with_:"bank = row[\"Name\"]"
  in
  let conflicting =
    Lua_csv_converter.create ~institution:"bunq" conflicting_source |> ok
  in
  let rows =
    header
    ^ "2025-01-01,2025-01-02,\"1,00\",one,,BankA,Text\n\
       2025-01-01,2025-01-02,\"1,00\",one,,BankB,Text\n"
  in
  Alcotest.(check bool)
    "conflicting account details are rejected"
    true
    (Result.is_error (Lua_csv_converter.convert conflicting ~name:"bad.csv" rows));
  List.iter
    [ header ^ "2025-01-01,2025-01-02,1.00,one,,,Text\n"
    ; header ^ "2025-01-01,2025-01-02,\"1,00\",bad account,,,Text\n"
    ]
    ~f:(fun contents ->
      Alcotest.(check bool)
        "old rejects invalid"
        true
        (Result.is_error (Csv_converter.convert old ~name:"bad.csv" contents));
      Alcotest.(check bool)
        "Lua rejects invalid"
        true
        (Result.is_error (Lua_csv_converter.convert lua ~name:"bad.csv" contents)))
;;

let bourso_equivalence () =
  let old =
    Csv_converter.of_string
      (Stdio.In_channel.read_all "fixtures/boursobank-converter.json")
    |> ok
  in
  let lua =
    Lua_csv_converter.create ~institution:"boursobank" (bundled "lua/boursobank.lua")
    |> ok
  in
  Alcotest.(check (list (pair string string)))
    "journal tags"
    (Csv_converter.journal_tags old)
    (Lua_csv_converter.tags lua);
  Alcotest.(check (list string))
    "conflict columns"
    (Csv_converter.conflict_columns old)
    (Lua_csv_converter.conflict_columns lua);
  let compare name contents =
    let normalize document =
      Records.merge_all [ document ] |> ok |> List.hd_exn |> Records.to_string
    in
    let expected = Csv_converter.convert old ~name contents |> ok |> normalize in
    let actual = Lua_csv_converter.convert lua ~name contents |> ok |> normalize in
    Alcotest.(check string) name expected actual
  in
  let ambiguous =
    String.substr_replace_first
      (bundled "lua/boursobank.lua")
      ~pattern:"\"Libellé\""
      ~with_:"\"Solde#2\""
  in
  Alcotest.(check bool)
    "reject ambiguous duplicate column aliases"
    true
    (Result.is_error (Lua_csv_converter.create ~institution:"boursobank" ambiguous));
  let invalid_column =
    String.substr_replace_first
      (bundled "lua/boursobank.lua")
      ~pattern:"\"Catégorie Parente\", \"Commentaire\" }"
      ~with_:"\"Catégorie Parente\", \"Missing\" }"
  in
  Alcotest.(check bool)
    "reject unknown conflict column"
    true
    (Result.is_error (Lua_csv_converter.create ~institution:"boursobank" invalid_column));
  compare "fixture.csv" (Stdio.In_channel.read_all "fixtures/bank.csv");
  let header =
    "Date Opération;Date Valeur;Libellé;Libellé Suggéré;Catégorie;Catégorie \
     Parente;Solde;Commentaire;Numéro Compte;Libellé Compte;Solde;Pointage\n"
  in
  compare "empty.csv" header;
  compare
    "accounts.csv"
    (header
     ^ "2025-01-02;2025-01-03; A ;A;C;P;-1 234,50;;111;Main;100.00;Non\n"
     ^ "2025-01-02;2025-01-03;B;;C;P;-1,00;;111;Main;100.00;Non\n"
     ^ "2025-01-04;2025-01-05;;C;C;P;2,00;;222;Main;;Non\n");
  List.iter
    [ "-1.00", "12.50"; "-1,00", "12,50" ]
    ~f:(fun (amount, balance) ->
      let contents =
        header
        ^ Printf.sprintf
            "2025-01-02;2025-01-02;Shop;Shop;C;P;%s;;111;Main;%s;Non\n"
            amount
            balance
      in
      Alcotest.(check bool)
        "legacy rejects invalid decimal"
        true
        (Result.is_error (Csv_converter.convert old ~name:"bad" contents));
      Alcotest.(check bool)
        "Lua rejects invalid decimal"
        true
        (Result.is_error (Lua_csv_converter.convert lua ~name:"bad" contents)))
;;

let revolut_equivalence () =
  let old =
    Csv_converter.of_string (Stdio.In_channel.read_all "fixtures/revolut-converter.json")
    |> ok
  in
  let lua =
    Lua_csv_converter.create ~institution:"revolut" (bundled "lua/revolut.lua") |> ok
  in
  let compare name contents =
    let expected = Csv_converter.convert old ~name contents |> ok |> Records.to_string in
    let actual =
      Lua_csv_converter.convert lua ~name contents |> ok |> Records.to_string
    in
    Alcotest.(check string) name expected actual
  in
  compare "fixture.csv" (Stdio.In_channel.read_all "fixtures/revolut.csv");
  let header =
    "Type,Product,Started Date,Completed \
     Date,Description,Amount,Fee,Currency,State,Balance\n"
  in
  compare "empty.csv" header;
  compare
    "reverted.csv"
    (header
     ^ "Deposit,Current,2026-01-01 09:00:00,2026-01-01 \
        09:00:01,In,150.00,0.00,EUR,COMPLETED,150.00\n"
     ^ "Card Payment,Current,2026-01-02 09:00:00,2026-01-03 \
        23:00:00,Shop,-10.00,1.00,EUR,COMPLETED,139.00\n"
     ^ "Card Payment,Current,2026-01-04 09:00:00,,Undone,-5.00,0.00,EUR,REVERTED,\n");
  compare
    "precision.csv"
    (header
     ^ "Deposit,Current,2026-01-01 09:00:00,2026-01-01 \
        09:00:01,In,12345678901234567890.123456789,0.000000001,USD,COMPLETED,12345678901234567890.123456788\n"
    );
  List.iter
    [ "Card,Current,2026-01-01 09:00:00,2026-01-01 08:00:00,Shop,1,0,EUR,COMPLETED,1\n"
    ; "Card,Current,2026-01-01 09:00:00,,Shop,x,0,EUR,REVERTED,\n"
    ; "Card,Current,2026-01-01 09:00:00,,Shop,1,0,EUR,REVERTED,1\n"
    ]
    ~f:(fun row ->
      let contents = header ^ row in
      Alcotest.(check bool)
        "old rejects invalid"
        true
        (Result.is_error (Csv_converter.convert old ~name:"bad" contents));
      Alcotest.(check bool)
        "Lua rejects invalid"
        true
        (Result.is_error (Lua_csv_converter.convert lua ~name:"bad" contents)))
;;

let wise_equivalence () =
  let old =
    Csv_converter.of_string (Stdio.In_channel.read_all "fixtures/wise-converter.json")
    |> ok
  in
  let source = bundled "lua/wise.lua" in
  let lua = Lua_csv_converter.create ~institution:"wise" source |> ok in
  Alcotest.(check (list (pair string string)))
    "journal tags"
    (Csv_converter.journal_tags old)
    (Lua_csv_converter.tags lua);
  Alcotest.(check (option string))
    "journal description"
    (Csv_converter.journal_description old)
    (Lua_csv_converter.journal_description lua);
  Alcotest.(check (list string))
    "conflict columns"
    (Csv_converter.conflict_columns old)
    (Lua_csv_converter.conflict_columns lua);
  let fixture = Stdio.In_channel.read_all "fixtures/wise.csv" in
  let compare name contents =
    let expected = Csv_converter.convert old ~name contents |> ok |> Records.to_string in
    let actual =
      Lua_csv_converter.convert lua ~name contents |> ok |> Records.to_string
    in
    Alcotest.(check string) name expected actual
  in
  compare "fixture.csv" fixture;
  let lines = String.split_lines fixture in
  let exchange =
    String.split (List.nth_exn lines 1) ~on:','
    |> List.mapi ~f:(fun i value ->
      match i with
      | 8 -> "EUR"
      | 9 -> "USD"
      | 10 -> "1.2345"
      | 11 -> "Payer"
      | 12 -> "Payee"
      | 13 -> "BE68539007547034"
      | 20 -> "12.34"
      | _ -> value)
    |> String.concat ~sep:","
  in
  let exchange_file =
    String.concat ~sep:"\n" (List.hd_exn lines :: exchange :: List.drop lines 2) ^ "\n"
  in
  compare "exchange.csv" exchange_file;
  compare
    "negative-rate.csv"
    (String.substr_replace_first exchange_file ~pattern:"1.2345" ~with_:"-1.2345");
  compare "empty.csv" (List.hd_exn (String.split_lines fixture) ^ "\n");
  compare
    "different-currency.csv"
    (String.substr_replace_all fixture ~pattern:",EUR," ~with_:",USD,");
  let cashback = "BALANCE_CASHBACK-abcdef01-2345-6789-abcd-ef0123456789" in
  let bare = "ABCDEF0123456789ABCDEF0123456789" in
  let changed =
    String.substr_replace_all fixture ~pattern:cashback ~with_:bare
    |> String.substr_replace_all ~pattern:"Synthetic shop" ~with_:"cashback"
  in
  compare "bare-reference.csv" changed;
  compare
    "upper-reference.csv"
    (String.substr_replace_all
       fixture
       ~pattern:cashback
       ~with_:"BALANCE_CASHBACK-ABCDEF01-2345-6789-ABCD-EF0123456789");
  let invalid =
    String.substr_replace_first
      fixture
      ~pattern:cashback
      ~with_:"BALANCE_CASHBACK-not-hex"
  in
  Alcotest.(check bool)
    "legacy rejects invalid reference"
    true
    (Result.is_error (Csv_converter.convert old ~name:"bad" invalid));
  Alcotest.(check bool)
    "Lua rejects invalid reference"
    true
    (Result.is_error (Lua_csv_converter.convert lua ~name:"bad" invalid));
  let invalid_description =
    String.substr_replace_first
      source
      ~pattern:"journal_description = \"Merchant\""
      ~with_:"journal_description = \"Missing\""
  in
  Alcotest.(check bool)
    "reject unknown description column"
    true
    (Result.is_error (Lua_csv_converter.create ~institution:"wise" invalid_description))
;;

let other_institution () =
  let source = Stdio.In_channel.read_all "fixtures/lua-example-bank.lua" in
  let converter = Lua_csv_converter.create ~institution:"example-bank" source |> ok in
  let document =
    Lua_csv_converter.convert
      converter
      ~name:"bank.csv"
      "Date,Description,Amount\n2026-01-02,Coffee,-3.50\n"
    |> ok
  in
  Alcotest.(check string) "institution" "example-bank" document.institution;
  let invalid_metadata =
    String.substr_replace_first
      source
      ~pattern:"converter = \"fin csv example-bank\""
      ~with_:"converter = \"\""
    |> Lua_csv_converter.create ~institution:"example-bank"
    |> ok
  in
  Alcotest.(check bool)
    "invalid converter metadata rejected"
    true
    (Result.is_error
       (Lua_csv_converter.convert
          invalid_metadata
          ~name:"bank.csv"
          "Date,Description,Amount\n2026-01-02,Coffee,-3.50\n"));
  Alcotest.(check bool)
    "duplicate row identities rejected"
    true
    (Result.is_error
       (Lua_csv_converter.convert
          converter
          ~name:"bank.csv"
          "Date,Description,Amount\n2026-01-02,Coffee,-3.50\n2026-01-02,Coffee,-3.50\n"));
  let row = List.hd_exn document.records in
  Alcotest.(check string)
    "amount"
    "-3.5"
    (Quantity.to_string (List.hd_exn row.movements).quantity)
;;

let () =
  Alcotest.run
    "Lua CSV"
    [ ( "converter"
      , [ Alcotest.test_case "CDC equivalence" `Quick compare_converters
        ; Alcotest.test_case "FDM account naming" `Quick fdm_account_naming
        ; Alcotest.test_case "Bunq equivalence" `Quick bunq_equivalence
        ; Alcotest.test_case "BoursoBank equivalence" `Quick bourso_equivalence
        ; Alcotest.test_case "Revolut equivalence" `Quick revolut_equivalence
        ; Alcotest.test_case "Wise equivalence" `Quick wise_equivalence
        ; Alcotest.test_case "another institution" `Quick other_institution
        ] )
    ]
;;
