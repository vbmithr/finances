open Base

type t =
  { institution : string
  ; bank_id : string
  ; account_id : string
  ; account_type : string
  ; iban : string
  }

let normalize_iban value =
  let value =
    String.filter value ~f:(fun c -> not (Char.is_whitespace c)) |> String.uppercase
  in
  let alnum c = Char.is_alpha c || Char.is_digit c in
  if
    String.length value < 15
    || String.length value > 34
    || (not (String.for_all value ~f:alnum))
    || not
         (Char.is_uppercase value.[0]
          && Char.is_uppercase value.[1]
          && Char.is_digit value.[2]
          && Char.is_digit value.[3])
  then
    Or_error.error_string
      "IBAN must contain a country code, two check digits and an alphanumeric account \
       identifier"
  else (
    let rotated = String.drop_prefix value 4 ^ String.prefix value 4 in
    let remainder =
      String.fold rotated ~init:0 ~f:(fun remainder c ->
        if Char.is_digit c
        then ((remainder * 10) + Char.to_int c - Char.to_int '0') % 97
        else ((remainder * 100) + Char.to_int c - Char.to_int 'A' + 10) % 97)
    in
    if remainder <> 1 then Or_error.error_string "IBAN checksum is invalid" else Ok value)
;;

let inferred_iban account_id = normalize_iban account_id |> Result.ok

let create ~institution ~bank_id ~account_id ~account_type ~iban =
  let open Or_error.Let_syntax in
  let%bind institution = Validation.text "institution" institution in
  let%bind bank_id = Validation.text "bank ID" bank_id in
  let%bind account_id = Validation.text "account ID" account_id in
  let%bind account_type = Validation.text "account type" account_type in
  let%map iban = normalize_iban iban in
  { institution; bank_id; account_id; account_type; iban }
;;

let source_account t =
  Yojson.Safe.to_string
    (`List [ `String t.bank_id; `String t.account_id; `String t.account_type ])
;;

let of_json contents =
  Or_error.try_with (fun () ->
    let open Yojson.Safe.Util in
    let rows =
      Yojson.Safe.from_string contents
      |> to_list
      |> List.filter_map ~f:(fun row ->
        let fields = to_assoc row in
        let names = List.map fields ~f:fst |> List.sort ~compare:String.compare in
        if
          not
            (List.equal
               String.equal
               names
               [ "account_id"; "account_type"; "bank_id"; "iban"; "institution" ])
        then
          failwith
            "identity requires exactly institution, bank_id, account_id, account_type \
             and iban";
        let field k = member k row |> to_string in
        let institution = field "institution"
        and bank_id = field "bank_id"
        and account_id = field "account_id"
        and account_type = field "account_type" in
        match member "iban" row with
        | `Null -> None
        | `String iban ->
          Some
            (create ~institution ~bank_id ~account_id ~account_type ~iban
             |> Or_error.ok_exn)
        | _ -> failwith "iban must be a string or null")
    in
    if
      List.contains_dup
        (List.map rows ~f:(fun r -> r.institution, source_account r))
        ~compare:[%compare: string * string]
    then failwith "duplicate account identity";
    rows)
;;
