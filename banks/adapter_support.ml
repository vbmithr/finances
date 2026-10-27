(** A source account identifier proves an IBAN only when it is one. *)
let own_iban account_id : Movement.own_iban =
  match Iban.of_string account_id with
  | Ok iban -> Known iban
  | Error _ -> Unknown
;;
