open Base

type t =
  { institution : string
  ; bank_id : string
  ; account_id : string
  ; account_type : string
  ; iban : Iban.t
  }

let inferred_iban account_id = Iban.of_string account_id |> Result.ok

let create ~institution ~bank_id ~account_id ~account_type ~iban =
  let open Or_error.Let_syntax in
  let%bind institution = Validation.text "institution" institution in
  let%bind bank_id = Validation.text "bank ID" bank_id in
  let%bind account_id = Validation.text "account ID" account_id in
  let%map account_type = Validation.text "account type" account_type in
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
          let iban = Iban.of_string iban |> Or_error.ok_exn in
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
