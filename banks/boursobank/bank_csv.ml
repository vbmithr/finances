open Base

type row =
  { row_number : int
  ; account_id : string
  ; booked_on : Date.t
  ; value_on : Date.t option
  ; amount : Quantity.t
  ; description : string
  ; suggested_description : string option
  ; category : string option
  ; parent_category : string option
  ; comment : string option
  ; account_label : string option
  ; reported_balance : Quantity.t option
  ; pointage : string option
  ; fields : string list
  }

let header =
  [ "Date Opération"
  ; "Date Valeur"
  ; "Libellé"
  ; "Libellé Suggéré"
  ; "Catégorie"
  ; "Catégorie Parente"
  ; "Solde"
  ; "Commentaire"
  ; "Numéro Compte"
  ; "Libellé Compte"
  ; "Solde"
  ; "Pointage"
  ]
;;

let schema =
  let open Csv_schema in
  make
    (fun
        row_number
         account_id
         booked_on
         value_on
         amount
         description
         suggested_description
         category
         parent_category
         comment
         account_label
         reported_balance
         pointage
         fields
       ->
       { row_number
       ; account_id
       ; booked_on
       ; value_on
       ; amount
       ; description
       ; suggested_description
       ; category
       ; parent_category
       ; comment
       ; account_label
       ; reported_balance
       ; pointage
       ; fields
       })
  |> row_number
  |> col ~pos:8 ~name:"Numéro Compte" (Cell.text ~name:"CSV account ID" ())
  |> col ~pos:0 ~name:"Date Opération" (Cell.iso_date ())
  |> col ~pos:1 ~name:"Date Valeur" (Cell.opt (Cell.iso_date ()))
  |> col ~pos:6 ~name:"Solde" Cell.french_amount
  |> col
       ~pos:2
       ~name:"Libellé"
       (Cell.required ~message:"CSV original description is required" Cell.string)
  |> col ~pos:3 ~name:"Libellé Suggéré" (Cell.opt Cell.string)
  |> col ~pos:4 ~name:"Catégorie" (Cell.opt Cell.string)
  |> col ~pos:5 ~name:"Catégorie Parente" (Cell.opt Cell.string)
  |> col ~pos:7 ~name:"Commentaire" (Cell.opt Cell.string)
  |> col ~pos:9 ~name:"Libellé Compte" (Cell.opt Cell.string)
  |> col ~pos:10 ~name:"Solde" (Cell.opt Cell.decimal_balance)
  |> col ~pos:11 ~name:"Pointage" (Cell.opt Cell.string)
  |> raw_fields
  |> create
       ~name:"BoursoBank"
       ~separator:';'
       ~header
       ~trim_header:true
       ~trim_cells:true
       ~skip_blank_rows:true
       ~columns_message:(fun expected _ ->
         Printf.sprintf "expected exactly %d CSV columns" expected)
;;

let recognizes = Csv_schema.recognizes schema
let of_fields = Csv_schema.of_fields schema
let parse = Csv_schema.parse schema
