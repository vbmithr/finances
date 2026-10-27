open Base
open Stdio
open Finances

let ok = Or_error.ok_exn
let rejected name result = Alcotest.(check bool) name true (Result.is_error result)

(* An adapter defined outside the banks library, as an external package would. *)
module Example = struct
  let institution = "example"
  let aliases = [ "examplebank" ]
  let kinds = [ "example-transaction"; "bank-transaction" ]

  let decode (event : Source_event.t) =
    Movement.create
      ~account:
        { bank_id = "example"
        ; account_id = event.account
        ; account_type = "current-EUR"
        ; currency = "EUR"
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
       ~payload)
;;

let construction () =
  let registry = ok (Registry.create (Builtin.adapters @ [ (module Example) ])) in
  Alcotest.(check (list string))
    "institutions"
    [ "boursobank"; "bunq"; "example"; "revolut"; "wise" ]
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
    [ "boursobank"; "boursorama" ]
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
        (Store.with_open ~registry:Builtin.registry path (fun db ->
           let report =
             ok
               (Store.import_bank_file
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
                ~substring:"known institutions: boursobank, bunq, revolut, wise");
           Alcotest.(check int) "no events" 0 (List.length (ok (Store.events db)));
           Ok ())))
;;

let () =
  Alcotest.run
    "registry"
    [ ( "registry"
      , [ Alcotest.test_case "construction" `Quick construction
        ; Alcotest.test_case "dispatch" `Quick dispatch
        ; Alcotest.test_case "aliases" `Quick aliases
        ; Alcotest.test_case "unregistered institution import" `Quick unregistered_import
        ] )
    ]
;;
