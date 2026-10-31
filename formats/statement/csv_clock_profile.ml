open Base

type columns =
  { reference : string
  ; date : string
  ; datetime : string
  ; amount : string
  ; currency : string
  ; description : string
  ; payment_reference : string
  ; running_balance : string
  ; payee : string
  ; merchant : string
  ; fees : string
  ; direction : string
  ; details_type : string
  }

type t =
  { institution : string
  ; name : string
  ; header : string list
  ; columns : columns
  ; conflict_columns : string list
  }

let columns_codec =
  Jsont.Object.map
    (fun
        reference
         date
         datetime
         amount
         currency
         description
         payment_reference
         running_balance
         payee
         merchant
         fees
         direction
         details_type
       ->
       { reference
       ; date
       ; datetime
       ; amount
       ; currency
       ; description
       ; payment_reference
       ; running_balance
       ; payee
       ; merchant
       ; fees
       ; direction
       ; details_type
       })
  |> Jsont.Object.mem "reference" Jsont.string ~enc:(fun p -> p.reference)
  |> Jsont.Object.mem "date" Jsont.string ~enc:(fun p -> p.date)
  |> Jsont.Object.mem "datetime" Jsont.string ~enc:(fun p -> p.datetime)
  |> Jsont.Object.mem "amount" Jsont.string ~enc:(fun p -> p.amount)
  |> Jsont.Object.mem "currency" Jsont.string ~enc:(fun p -> p.currency)
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun p -> p.description)
  |> Jsont.Object.mem "payment_reference" Jsont.string ~enc:(fun p -> p.payment_reference)
  |> Jsont.Object.mem "running_balance" Jsont.string ~enc:(fun p -> p.running_balance)
  |> Jsont.Object.mem "payee" Jsont.string ~enc:(fun p -> p.payee)
  |> Jsont.Object.mem "merchant" Jsont.string ~enc:(fun p -> p.merchant)
  |> Jsont.Object.mem "fees" Jsont.string ~enc:(fun p -> p.fees)
  |> Jsont.Object.mem "direction" Jsont.string ~enc:(fun p -> p.direction)
  |> Jsont.Object.mem "details_type" Jsont.string ~enc:(fun p -> p.details_type)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let codec =
  Jsont.Object.map (fun () institution name header columns conflict_columns ->
    let institution = Validation.token "institution" institution |> Json.or_error in
    let name = Validation.text "CSV profile name" name |> Json.or_error in
    if List.is_empty header then failwith "CSV profile header is empty";
    if List.contains_dup header ~compare:String.compare
    then failwith "CSV profile has duplicate header names";
    let required =
      [ columns.reference
      ; columns.date
      ; columns.datetime
      ; columns.amount
      ; columns.currency
      ; columns.description
      ; columns.payment_reference
      ; columns.running_balance
      ; columns.payee
      ; columns.merchant
      ; columns.fees
      ; columns.direction
      ; columns.details_type
      ]
      @ conflict_columns
    in
    List.iter required ~f:(fun column ->
      if not (List.mem header column ~equal:String.equal)
      then failwith ("missing CSV profile column: " ^ column));
    { institution; name; header; columns; conflict_columns })
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun p -> p.institution)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun p -> p.name)
  |> Jsont.Object.mem "header" (Jsont.list Jsont.string) ~enc:(fun p -> p.header)
  |> Jsont.Object.mem "columns" columns_codec ~enc:(fun p -> p.columns)
  |> Jsont.Object.mem "conflict_columns" (Jsont.list Jsont.string) ~enc:(fun p ->
    p.conflict_columns)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let profiles =
  Json.decode (Jsont.list codec) Csv_clock_profiles_data.json |> Or_error.ok_exn
;;

let find institution =
  List.find profiles ~f:(fun p -> String.equal p.institution institution)
;;
