open Base

let ok = Or_error.ok_exn
let check name expected actual = Alcotest.(check string) name expected actual

let rejects name reason value =
  match Iban.of_string value with
  | Ok _ -> Alcotest.failf "%s: accepted" name
  | Error e ->
    let message = Error.to_string_hum e in
    Alcotest.(check bool)
      (name ^ ": " ^ message)
      true
      (String.is_substring message ~substring:reason)
;;

let registry_examples () =
  Alcotest.(check int) "registered countries" 89 (List.length Iban.Registry.entries);
  Alcotest.(check bool)
    "sorted and unique"
    true
    (List.is_sorted_strictly Iban.Registry.entries ~compare:(fun a b ->
       String.compare a.Iban.Registry.country b.country));
  List.iter Iban.Registry.entries ~f:(fun entry ->
    let iban = ok (Iban.of_string entry.example) in
    check (entry.country ^ " electronic") entry.example (Iban.to_string iban);
    check
      (entry.country ^ " print round trip")
      entry.example
      (Iban.to_string (ok (Iban.of_string (Iban.to_print iban))));
    check
      (entry.country ^ " check digits recomputed")
      entry.example
      (Iban.to_string (ok (Iban.create ~country:entry.country ~bban:(Iban.bban iban))));
    Alcotest.(check int)
      (entry.country ^ " length")
      entry.length
      (String.length entry.example))
;;

let formats () =
  let iban = ok (Iban.of_string "gb82 west\t1234 5698 7654 32") in
  check "electronic" "GB82WEST12345698765432" (Iban.to_string iban);
  check "print" "GB82 WEST 1234 5698 7654 32" (Iban.to_print iban);
  check "country" "GB" (Iban.country iban);
  check "check digits" "82" (Iban.check_digits iban);
  check "BBAN" "WEST12345698765432" (Iban.bban iban);
  Alcotest.(check (option string)) "bank" (Some "WEST") (Iban.bank_identifier iban);
  Alcotest.(check (option string)) "branch" (Some "123456") (Iban.branch_identifier iban);
  let de = ok (Iban.of_string "DE89370400440532013000") in
  Alcotest.(check (option string)) "DE bank" (Some "37040044") (Iban.bank_identifier de);
  Alcotest.(check (option string)) "DE branch" None (Iban.branch_identifier de);
  Alcotest.(check bool) "DE SEPA" true (Iban.is_sepa de);
  let br = ok (Iban.of_string "BR6699999A03000010009795493C1") in
  Alcotest.(check bool) "BR not SEPA" false (Iban.is_sepa br);
  Alcotest.(check (option string))
    "BR alphanumeric bank (release 103)"
    (Some "99999A03")
    (Iban.bank_identifier br);
  Alcotest.(check bool)
    "equal ignores input format"
    true
    (Iban.equal iban (ok (Iban.of_string "GB82WEST12345698765432")))
;;

(* Replace the check digits of a valid IBAN, keeping everything else. *)
let with_digits iban digits = Iban.country iban ^ digits ^ Iban.bban iban

(* MOD 97-10 alone cannot tell 00 from 97, 01 from 98 or 99 from 02. *)
let unissued_check_digits () =
  let found = Hashtbl.create (module String) in
  let rec search n =
    if Hashtbl.length found < 3 && n < 100_000
    then (
      let iban =
        ok (Iban.create ~country:"GB" ~bban:(Printf.sprintf "WEST12345698%06d" n))
      in
      (match Iban.check_digits iban with
       | "97" -> Hashtbl.set found ~key:"00" ~data:iban
       | "98" -> Hashtbl.set found ~key:"01" ~data:iban
       | "02" -> Hashtbl.set found ~key:"99" ~data:iban
       | _ -> ());
      search (n + 1))
  in
  search 0;
  List.iter [ "00"; "01"; "99" ] ~f:(fun digits ->
    let iban = Hashtbl.find_exn found digits in
    rejects ("check digits " ^ digits) "between 02 and 98" (with_digits iban digits))
;;

let rejections () =
  rejects "empty" "only letters and digits" "";
  rejects "punctuation" "only letters and digits" "GB82-WEST-1234-5698-7654-32";
  rejects "account label" "two-letter country code" "main";
  rejects "unknown country" "not in the IBAN Registry" "XX82WEST12345698765432";
  rejects "short" "must have 22 characters, not 21" "GB82WEST1234569876543";
  rejects "long" "must have 22 characters, not 23" "GB82WEST123456987654321";
  (* Digits where GB requires bank letters; structure is reported before the
     checksum is considered. *)
  rejects
    "structure"
    "does not match the registered structure 4!a6!n8!n"
    "GB82370400440532013000";
  rejects "checksum" "checksum is invalid" "GB83WEST12345698765432";
  rejects "Belgian checksum" "checksum is invalid" "BE69539007547034";
  (match Iban.create ~country:"GB" ~bban:"1234" with
   | Ok _ -> Alcotest.fail "short BBAN accepted"
   | Error _ -> ());
  match Iban.of_string "GB83WEST12345698765432" with
  | Ok _ -> Alcotest.fail "accepted"
  | Error e ->
    Alcotest.(check bool)
      "error omits the account number"
      false
      (String.is_substring (Error.to_string_hum e) ~substring:"12345698765432")
;;

let registry_facts () =
  Alcotest.(check bool)
    "release recorded"
    true
    (String.is_prefix Iban.Registry.release ~prefix:"Release ");
  let br = Option.value_exn (Iban.Registry.find "BR") in
  check "BR structure" "8!c5!n10!n1!c1!c" (Iban.Registry.structure br);
  let fr = Option.value_exn (Iban.Registry.find "FR") in
  check "FR structure" "5!n5!n11!c2!n" (Iban.Registry.structure fr);
  Alcotest.(check int) "FR length" 27 fr.length;
  Alcotest.(check bool) "unknown" true (Option.is_none (Iban.Registry.find "XX"))
;;

let () =
  Alcotest.run
    "IBAN"
    [ ( "iban"
      , [ Alcotest.test_case "registry examples" `Quick registry_examples
        ; Alcotest.test_case "formats and identifiers" `Quick formats
        ; Alcotest.test_case "unissued check digits" `Quick unissued_check_digits
        ; Alcotest.test_case "rejections" `Quick rejections
        ; Alcotest.test_case "registry facts" `Quick registry_facts
        ] )
    ]
;;
