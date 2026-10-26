open Core

type row =
  { row_number : int
  ; account_id : string
  ; booked_on : Date.t
  ; value_on : Date.t option
  ; amount : Bigdecimal.t
  ; description : string
  ; suggested_description : string option
  ; category : string option
  ; parent_category : string option
  ; comment : string option
  ; account_label : string option
  ; reported_balance : Bigdecimal.t option
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

let without_bom s = Option.value (String.chop_prefix s ~prefix:"\239\187\191") ~default:s

let channel s =
  Csv.of_string
    ~separator:';'
    ~strip:false
    ~backslash_escape:false
    ~excel_tricks:false
    ~fix:false
    (without_bom s)
;;

let recognizes contents =
  match Or_error.try_with (fun () -> Csv.next (channel contents)) with
  | Ok fields -> List.equal String.equal (List.map fields ~f:String.strip) header
  | Error _ -> false
;;

let amount_syntax = Re.Perl.compile_pat "^[+-]?([0-9]+|[0-9]{1,3}( [0-9]{3})+)(,[0-9]+)?$"
let balance_syntax = Re.Perl.compile_pat "^[+-]?[0-9]+(\\.[0-9]+)?$"
let date_syntax = Re.Perl.compile_pat "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"

let date value =
  if not (Re.execp date_syntax value) then failwith "expected an ISO calendar date";
  Date.of_string value
;;

let amount value =
  let value =
    String.substr_replace_all value ~pattern:"\194\160" ~with_:" "
    |> fun s -> String.substr_replace_all s ~pattern:"\226\128\175" ~with_:" "
  in
  if not (Re.execp amount_syntax value) then failwith "invalid French movement amount";
  value
  |> String.filter ~f:(fun c -> not (Char.equal c ' '))
  |> String.map ~f:(fun c -> if Char.equal c ',' then '.' else c)
  |> Bigdecimal.of_string
;;

let balance value =
  if not (Re.execp balance_syntax value) then failwith "invalid decimal account balance";
  Bigdecimal.of_string value
;;

let of_fields ~row_number fields =
  Or_error.try_with (fun () ->
    if row_number <= 0 then failwith "CSV row number must be positive";
    if List.length fields <> 12 then failwith "expected exactly 12 CSV columns";
    let values = Array.of_list (List.map fields ~f:String.strip) in
    let optional i = if String.is_empty values.(i) then None else Some values.(i) in
    let account_id = Validation.text "CSV account ID" values.(8) |> Or_error.ok_exn in
    if String.is_empty values.(2) then failwith "CSV original description is required";
    { row_number
    ; account_id
    ; booked_on = date values.(0)
    ; value_on = Option.map (optional 1) ~f:date
    ; amount = amount values.(6)
    ; description = values.(2)
    ; suggested_description = optional 3
    ; category = optional 4
    ; parent_category = optional 5
    ; comment = optional 7
    ; account_label = optional 9
    ; reported_balance = Option.map (optional 10) ~f:balance
    ; pointage = optional 11
    ; fields
    })
  |> fun result -> Or_error.tag result ~tag:(sprintf "CSV row %d" row_number)
;;

let parse contents =
  Or_error.try_with (fun () ->
    if not (Stdlib.String.is_valid_utf_8 contents)
    then failwith "BoursoBank CSV must be UTF-8";
    let input = channel contents in
    if not (List.equal String.equal (List.map (Csv.next input) ~f:String.strip) header)
    then failwith "unsupported BoursoBank CSV header";
    Csv.input_all input
    |> List.mapi ~f:(fun index fields ->
      if List.for_all fields ~f:(fun s -> String.is_empty (String.strip s))
      then None
      else Some (of_fields ~row_number:(index + 1) fields |> Or_error.ok_exn))
    |> List.filter_opt)
  |> fun result -> Or_error.tag result ~tag:"BoursoBank CSV"
;;
