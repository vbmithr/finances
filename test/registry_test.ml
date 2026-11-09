open Base
open Stdio
open Finances
open Finances_store

let ok = Or_error.ok_exn
let rejected name result = Alcotest.(check bool) name true (Result.is_error result)

(* An adapter defined outside the banks library, as an external package would. *)
module Example = struct
  let institution = "example"
  let aliases = [ "examplebank" ]
  let kinds = [ "example-transaction"; "bank-transaction" ]
  let operation_kinds = []
  let operation_rows _ = Or_error.error_string "example produces no operation events"
  let formats = [ "example" ]
  let probe ~filename:_ _ = None
  let takes_account_label = false
  let reports_tickers = false
  let own_symbols = []
  let parse ~context:_ ~format:_ _ = Ok Document.empty
  let overlap = Adapter.Source_ids
  let companions = []

  let decode (event : Source_event.t) =
    Movement.create
      ~account:
        { bank_id = "example"
        ; account_id = event.account
        ; account_type = "current-EUR"
        ; currency = Option.value_exn (Iso4217.of_alpha_code "EUR")
        }
      ~own_iban:Unknown
      ~booked_on:(Date.of_string "2026-10-27")
      ~amount:(Quantity.of_string event.payload)
      [ "example " ^ event.source_id ]
  ;;
end

let event ?(venue = "example") ?(kind = "example-transaction") payload =
  ok
    (Source_event.create_dated
       ~venue
       ~account:"main"
       ~kind
       ~source_id:"1"
       ~occurred_on:(Date.of_string "2026-10-27")
       ~payload
       ())
;;

let construction () =
  let registry = ok (Registry.create (Builtin.adapters @ [ (module Example) ])) in
  Alcotest.(check (list string))
    "institutions"
    [ "boursobank"
    ; "bunq"
    ; "cdc"
    ; "ethereum"
    ; "example"
    ; "fdm"
    ; "kraken"
    ; "polygon"
    ; "revolut"
    ; "wise"
    ]
    (Registry.institutions registry);
  rejected
    "duplicate institution"
    (Registry.create [ (module Example); (module Example) ]);
  rejected
    "no kinds"
    (Registry.create
       [ (module struct
           include Example

           let kinds = []
         end)
       ]);
  rejected
    "invalid institution"
    (Registry.create
       [ (module struct
           include Example

           let institution = "two words"
         end)
       ])
;;

let dispatch () =
  let registry = ok (Registry.create (Builtin.adapters @ [ (module Example) ])) in
  let movement = ok (Registry.movement registry (event "12.5")) in
  Alcotest.(check string)
    "decoded by its adapter"
    "12.5"
    (Quantity.to_string movement.amount);
  Alcotest.(check bool)
    "unclaimed kind is not a movement"
    false
    (Registry.is_movement registry (event ~kind:"perp-fill" "1"));
  rejected
    "bank kind under an unregistered venue"
    (Registry.movement registry (event ~venue:"elsewhere" "1"));
  rejected
    "kind the venue's adapter does not produce"
    (Registry.movement registry (event ~venue:"wise" "1"));
  (* Two institutions share the OFX kind; each decodes its own events. *)
  let shared = ok (Registry.movement registry (event ~kind:"bank-transaction" "3")) in
  Alcotest.(check (list string)) "shared kind" [ "example 1" ] shared.descriptions;
  rejected
    "the shared kind still goes to the venue's adapter"
    (Registry.movement registry (event ~venue:"boursobank" ~kind:"bank-transaction" "3"))
;;

let aliases () =
  let registry = ok (Registry.create (Builtin.adapters @ [ (module Example) ])) in
  Alcotest.(check (list string))
    "registered"
    [ "boursobank"; "boursorama"; "bourso" ]
    (Registry.aliases registry "boursobank");
  Alcotest.(check (list string))
    "external"
    [ "example"; "examplebank" ]
    (Registry.aliases registry "example");
  Alcotest.(check (list string))
    "unregistered"
    [ "elsewhere" ]
    (Registry.aliases registry "elsewhere")
;;

let unregistered_import () =
  let path =
    Stdlib.Filename.temp_file
      ~temp_dir:(Sys.getenv_exn "TMPDIR")
      "finances-registry-"
      ".sqlite3"
  in
  Exn.protect
    ~finally:(fun () -> Stdlib.Sys.remove path)
    ~f:(fun () ->
      ok
        (Setup.with_open ~registry:Builtin.registry path (fun db ->
           let report =
             ok
               (Import.import_bank_file
                  db
                  ~institution:"elsewhere"
                  ~path:"bank.ofx"
                  ~contents:(In_channel.read_all "fixtures/bank.ofx"))
           in
           Alcotest.(check string)
             "rejected"
             "rejected"
             (Bank_import.outcome_name report.outcome);
           Alcotest.(check bool)
             "names the known institutions"
             true
             (String.is_substring
                (Option.value_exn report.message)
                ~substring:
                  "known institutions: boursobank, bunq, cdc, ethereum, fdm, kraken, \
                   polygon, revolut, wise");
           Alcotest.(check int) "no events" 0 (List.length (ok (Ledger.events db)));
           Ok ())))
;;

(* The bundled institutions are one definition file each. *)
let bundled_definitions () =
  let raw =
    [ "boursobank", Bundled_definitions.boursobank
    ; "wise", Bundled_definitions.wise
    ; "revolut", Bundled_definitions.revolut
    ; "bunq", Bundled_definitions.bunq
    ; "cdc", Bundled_definitions.cdc
    ; "kraken", Bundled_definitions.kraken
    ; "fdm", Bundled_definitions.fdm
    ; "ethereum", Bundled_definitions.ethereum
    ; "polygon", Bundled_definitions.polygon
    ]
  in
  Alcotest.(check (list string))
    "institutions"
    (List.map raw ~f:fst)
    (List.map Builtin.definitions ~f:Institution_definition.institution);
  List.iter2_exn raw Builtin.definitions ~f:(fun (name, text) definition ->
    let decoded =
      Institution_toml.decode text ~load:(fun path ->
        List.Assoc.find Bundled_definitions.csv path ~equal:String.equal
        |> Or_error.of_option ~error:(Error.of_string "missing CSV layout"))
      |> Or_error.ok_exn
    in
    Alcotest.(check string)
      (name ^ " TOML round trip")
      (Json.encode Institution_definition.codec definition)
      (Json.encode Institution_definition.codec decoded);
    let file =
      Toml.of_string Institution_toml.file_codec text
      |> Result.map_error ~f:(fun e -> Error.of_string (Toml.Error.to_string e))
      |> Or_error.ok_exn
    in
    let encoded = Toml.to_string Institution_toml.file_codec file in
    let roundtrip =
      Institution_toml.decode encoded ~load:(fun path ->
        List.Assoc.find Bundled_definitions.csv path ~equal:String.equal
        |> Or_error.of_option ~error:(Error.of_string "missing CSV layout"))
      |> Or_error.ok_exn
    in
    Alcotest.(check string)
      (name ^ " TOML codec encoding round trip")
      (Json.encode Institution_definition.codec definition)
      (Json.encode Institution_definition.codec roundtrip);
    (* A bundled rule keeps its own name and priority when installed. *)
    List.iter definition.rules ~f:(fun (r : Institution_definition.rule) ->
      Alcotest.(check bool)
        (name ^ " rule names start with the institution")
        true
        (String.is_prefix r.name ~prefix:(name ^ "-")));
    ());
  let names =
    List.concat_map Builtin.definitions ~f:(fun d ->
      List.map d.rules ~f:(fun r -> r.name))
  in
  Alcotest.(check bool)
    "rule names are unique across bundled institutions"
    false
    (List.contains_dup names ~compare:String.compare);
  let derived = List.length Builtin.source_evidence + List.length Builtin.type_rules in
  Alcotest.(check int)
    "every bundled rule becomes a default"
    (List.sum (module Int) Builtin.definitions ~f:(fun d -> List.length d.rules))
    derived
;;

let wallet_networks () =
  let parse text = Json.decode Institution_definition.codec text in
  let definition typ extra =
    Printf.sprintf
      {|{"version":1,"kind":"records","type":"%s","records":{"institution":"example","ofx":true%s}}|}
      typ
      extra
  in
  let evm = ok (parse (definition "wallet" {|,"chain_id":137|})) in
  Alcotest.(check bool)
    "wallet carries EVM chain"
    true
    (Institution_type.equal evm.typ (Wallet (Evm { chain_id = 137 })));
  Alcotest.(check (option int))
    "chain lookup"
    (Some 137)
    (Institution_definition.chain_id evm);
  let other_chain = ok (parse (definition "wallet" {|,"chain_id":1|})) in
  Alcotest.(check bool)
    "changing network changes source"
    false
    (Institution_definition.same_sources evm other_chain);
  let bank = ok (parse (definition "bank" "")) in
  let exchange = ok (parse (definition "crypto-exchange" "")) in
  Alcotest.(check bool)
    "changing category alone leaves source unchanged"
    true
    (Institution_definition.same_sources bank exchange);
  let encoded = Json.encode Institution_definition.codec evm in
  let roundtrip = ok (parse encoded) in
  Alcotest.(check bool)
    "EVM roundtrip"
    true
    (Institution_type.equal roundtrip.typ evm.typ);
  let other = ok (parse (definition "wallet" {|,"network":"bitcoin"|})) in
  Alcotest.(check bool)
    "other network roundtrip"
    true
    (Institution_type.equal
       (ok (parse (Json.encode Institution_definition.codec other))).typ
       (Wallet (Other "bitcoin")));
  let unknown = ok (parse (definition "wallet" "")) in
  Alcotest.(check bool)
    "unspecified wallet"
    true
    (Institution_type.equal unknown.typ (Wallet Unspecified));
  rejected "bank cannot name a chain" (parse (definition "bank" {|,"chain_id":1|}));
  rejected
    "exchange cannot name a network"
    (parse (definition "crypto-exchange" {|,"network":"bitcoin"|}));
  rejected
    "network choices cannot be combined"
    (parse (definition "wallet" {|,"chain_id":1,"network":"bitcoin"|}));
  rejected "empty network name" (parse (definition "wallet" {|,"network":""|}))
;;

let () =
  Alcotest.run
    "registry"
    [ ( "registry"
      , [ Alcotest.test_case "construction" `Quick construction
        ; Alcotest.test_case "dispatch" `Quick dispatch
        ; Alcotest.test_case "aliases" `Quick aliases
        ; Alcotest.test_case "unregistered institution import" `Quick unregistered_import
        ; Alcotest.test_case "bundled definitions" `Quick bundled_definitions
        ; Alcotest.test_case "wallet networks" `Quick wallet_networks
        ] )
    ]
;;
