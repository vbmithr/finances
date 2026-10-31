open Base

type columns =
  { transaction_type : string
  ; product : string
  ; started : string
  ; completed : string
  ; description : string
  ; amount : string
  ; fee : string
  ; currency : string
  ; state : string
  ; balance : string
  }

type t =
  { institution : string
  ; name : string
  ; aliases : string list
  ; format : string
  ; kind : string
  ; payload_tag : string
  ; account_prefix : string
  ; header : string list
  ; columns : columns
  ; product_value : string
  ; completed_value : string
  ; reverted_value : string
  }

let columns_codec =
  Jsont.Object.map
    (fun
        transaction_type
         product
         started
         completed
         description
         amount
         fee
         currency
         state
         balance
       ->
       { transaction_type
       ; product
       ; started
       ; completed
       ; description
       ; amount
       ; fee
       ; currency
       ; state
       ; balance
       })
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun p -> p.transaction_type)
  |> Jsont.Object.mem "product" Jsont.string ~enc:(fun p -> p.product)
  |> Jsont.Object.mem "started" Jsont.string ~enc:(fun p -> p.started)
  |> Jsont.Object.mem "completed" Jsont.string ~enc:(fun p -> p.completed)
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun p -> p.description)
  |> Jsont.Object.mem "amount" Jsont.string ~enc:(fun p -> p.amount)
  |> Jsont.Object.mem "fee" Jsont.string ~enc:(fun p -> p.fee)
  |> Jsont.Object.mem "currency" Jsont.string ~enc:(fun p -> p.currency)
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun p -> p.state)
  |> Jsont.Object.mem "balance" Jsont.string ~enc:(fun p -> p.balance)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let codec =
  Jsont.Object.map
    (fun
        ()
         institution
         name
         aliases
         format
         kind
         payload_tag
         account_prefix
         header
         columns
         product_value
         completed_value
         reverted_value
       ->
       let institution = Validation.token "institution" institution |> Json.or_error in
       let name = Validation.text "profile name" name |> Json.or_error in
       let aliases =
         List.map aliases ~f:(fun a -> Validation.token "alias" a |> Json.or_error)
       in
       let format = Validation.token "format" format |> Json.or_error in
       let kind = Validation.token "kind" kind |> Json.or_error in
       let payload_tag = Validation.token "payload tag" payload_tag |> Json.or_error in
       let _ = Validation.text "account prefix" account_prefix |> Json.or_error in
       List.iter [ product_value; completed_value; reverted_value ] ~f:(fun value ->
         ignore (Validation.text "CSV source value" value |> Json.or_error : string));
       if List.contains_dup header ~compare:String.compare
       then failwith "duplicate CSV header";
       List.iter
         [ columns.transaction_type
         ; columns.product
         ; columns.started
         ; columns.completed
         ; columns.description
         ; columns.amount
         ; columns.fee
         ; columns.currency
         ; columns.state
         ; columns.balance
         ]
         ~f:(fun c ->
           if not (List.mem header c ~equal:String.equal)
           then failwith ("missing CSV column: " ^ c));
       if String.equal completed_value reverted_value
       then failwith "completed and reverted states must differ";
       { institution
       ; name
       ; aliases
       ; format
       ; kind
       ; payload_tag
       ; account_prefix
       ; header
       ; columns
       ; product_value
       ; completed_value
       ; reverted_value
       })
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun p -> p.institution)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun p -> p.name)
  |> Jsont.Object.mem "aliases" (Jsont.list Jsont.string) ~enc:(fun p -> p.aliases)
  |> Jsont.Object.mem "format" Jsont.string ~enc:(fun p -> p.format)
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun p -> p.kind)
  |> Jsont.Object.mem "payload_tag" Jsont.string ~enc:(fun p -> p.payload_tag)
  |> Jsont.Object.mem "account_prefix" Jsont.string ~enc:(fun p -> p.account_prefix)
  |> Jsont.Object.mem "header" (Jsont.list Jsont.string) ~enc:(fun p -> p.header)
  |> Jsont.Object.mem "columns" columns_codec ~enc:(fun p -> p.columns)
  |> Jsont.Object.mem "product_value" Jsont.string ~enc:(fun p -> p.product_value)
  |> Jsont.Object.mem "completed_value" Jsont.string ~enc:(fun p -> p.completed_value)
  |> Jsont.Object.mem "reverted_value" Jsont.string ~enc:(fun p -> p.reverted_value)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let all = Json.decode (Jsont.list codec) Csv_running_profiles_data.json |> Or_error.ok_exn
let find institution = List.find all ~f:(fun p -> String.equal p.institution institution)
