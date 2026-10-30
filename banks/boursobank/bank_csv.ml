type row = Csv_companion_layout.row

let profile =
  Json.decode Csv_companion_layout.codec Boursobank_companion_default.json
  |> Base.Or_error.ok_exn
;;

let schema = Csv_companion_layout.schema profile
let recognizes = Csv_schema.recognizes schema
let of_fields = Csv_schema.of_fields schema
let parse = Csv_schema.parse schema
