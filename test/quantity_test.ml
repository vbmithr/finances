open Base
open Finances

let ok = Or_error.ok_exn
let check name expected actual = Alcotest.(check bool) name expected actual

let exact_arithmetic () =
  let original = !Decimal.Context.default in
  Exn.protect
    ~finally:(fun () -> Decimal.Context.default := original)
    ~f:(fun () ->
      Decimal.Context.default := Decimal.Context.make ~prec:2 ~e_max:9 ~e_min:(-9) ();
      let values =
        List.map
          [ "0"
          ; "-0.00"
          ; "1"
          ; "-1"
          ; "0.1"
          ; "0.01"
          ; "9.99"
          ; "-9.99"
          ; "1e40"
          ; "1e-40"
          ; "-1e40"
          ; "1.000000000000000000000000000001"
          ; "1234567890123456789012345678901234567890.1234567890123456789"
          ]
          ~f:Quantity.of_string
      in
      List.iter values ~f:(fun a ->
        List.iter values ~f:(fun b ->
          let rational value = Decimal.to_rational value in
          List.iter
            [ Quantity.add, Q.add; Quantity.sub, Q.sub; Quantity.mul, Q.mul ]
            ~f:(fun (operation, reference) ->
              check
                "exact rational result"
                true
                (Q.equal (rational (operation a b)) (reference (rational a) (rational b))))));
      let huge =
        Quantity.of_string "123456789012345678901234567890.1234567890123456789"
      in
      let debit = ok (Accounting.posting ~account:"assets:bank" ~commodity:"EUR" huge) in
      let credit =
        ok
          (Accounting.posting ~account:"equity:test" ~commodity:"EUR" (Quantity.neg huge))
      in
      ignore
        (ok (Accounting.entry ~date:(2026, 1, 1) ~description:"Exact" [ debit; credit ])))
;;

let canonical_storage () =
  List.iter
    [ "-0.00", "0"; "1.000", "1"; "1e3", "1000"; "0.0001000", "0.0001" ]
    ~f:(fun (source, expected) ->
      Alcotest.(check string)
        "canonical decimal"
        expected
        (Quantity.to_string (Quantity.of_string source)));
  Alcotest.(check int)
    "normalized scale"
    (-2)
    (Quantity.exponent (Quantity.of_string "12.5000e-1"));
  List.iter [ Decimal.nan; Decimal.infinity; Decimal.neg_infinity ] ~f:(fun quantity ->
    check
      "non-finite posting rejected"
      true
      (Result.is_error
         (Accounting.posting ~account:"assets:bank" ~commodity:"EUR" quantity)))
;;

let calendar_and_clock () =
  Alcotest.(check int) "leap-day difference" 2 (Date.diff (2024, 3, 1) (2024, 2, 28));
  Alcotest.(check int) "reverse difference" (-2) (Date.diff (2024, 2, 28) (2024, 3, 1));
  check
    "invalid source date rejected"
    true
    (Result.is_error
       (Source_event.create_dated
          ~venue:"test"
          ~account:"main"
          ~kind:"test"
          ~source_id:"1"
          ~occurred_on:(2023, 2, 29)
          ~payload:"test"));
  let ns source =
    Timestamp.of_rfc3339_exn source |> Timestamp.nanoseconds_since_epoch_exn
  in
  Alcotest.(check int64)
    "nanosecond precision"
    123456789L
    (ns "1970-01-01T00:00:00.123456789Z");
  Alcotest.(check int64) "timezone offset" 0L (ns "1970-01-01T01:00:00+01:00");
  check
    "pre-epoch rejected"
    true
    (Result.is_error (Or_error.try_with (fun () -> ns "1969-12-31T23:59:59Z")))
;;

let () =
  Alcotest.run
    "Decimal and calendar migration"
    [ ( "invariants"
      , List.map
          [ "exact arithmetic", exact_arithmetic
          ; "canonical storage and finite values", canonical_storage
          ; "calendar and timestamps", calendar_and_clock
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
