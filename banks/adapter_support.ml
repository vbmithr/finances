open Base

(** A source account identifier proves an IBAN only when it is one. *)
let own_iban account_id : Movement.own_iban =
  match Iban.of_string account_id with
  | Ok iban -> Known iban
  | Error _ -> Unknown
;;

(** The start of a document, for format detection. *)
let prefix contents = String.prefix contents 4096 |> String.strip

let extension filename = String.lowercase (Stdlib.Filename.extension filename)

let rows_of parse contents ~number ~fields =
  Or_error.map (parse contents) ~f:(List.map ~f:(fun row -> number row, fields row))
;;

let optional f = Option.map ~f
let date d = Date.to_string d
let quantity q = Quantity.to_string q
