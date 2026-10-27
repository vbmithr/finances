open Base

type reference =
  { institution : string
  ; bank_id : string
  ; account_id : string
  ; account_type : string
  }

type 'a update =
  | Keep
  | Set of 'a
  | Clear

type t =
  { reference : reference
  ; ledger_account : string option
  ; iban : Iban.t update
  ; settlement_account : reference update
  }

let reference ~institution ~bank_id ~account_id ~account_type =
  let open Or_error.Let_syntax in
  let%bind institution = Validation.text "institution" institution in
  let%bind bank_id = Validation.text "bank_id" bank_id in
  let%bind account_id = Validation.text "account_id" account_id in
  let%map account_type = Validation.text "account_type" account_type in
  { institution; bank_id; account_id; account_type }
;;

let source_account r =
  Yojson.Safe.to_string
    (`List [ `String r.bank_id; `String r.account_id; `String r.account_type ])
;;

let reference_fields r =
  [ "institution", `String r.institution
  ; "bank_id", `String r.bank_id
  ; "account_id", `String r.account_id
  ; "account_type", `String r.account_type
  ]
;;

let reference_to_json r = `Assoc (reference_fields r)

let reference_of_json json =
  Or_error.try_with (fun () ->
    let open Yojson.Safe.Util in
    let fields = to_assoc json in
    if
      not
        (List.equal
           String.equal
           (List.map fields ~f:fst |> List.sort ~compare:String.compare)
           [ "account_id"; "account_type"; "bank_id"; "institution" ])
    then
      failwith
        "account reference requires institution, bank_id, account_id and account_type";
    let field k = member k json |> to_string in
    reference
      ~institution:(field "institution")
      ~bank_id:(field "bank_id")
      ~account_id:(field "account_id")
      ~account_type:(field "account_type")
    |> Or_error.ok_exn)
;;

let of_json contents =
  Or_error.try_with (fun () ->
    let open Yojson.Safe.Util in
    let rows =
      match Yojson.Safe.from_string contents with
      | `Assoc fields as json ->
        if
          (not
             (List.equal
                String.equal
                (List.map fields ~f:fst |> List.sort ~compare:String.compare)
                [ "accounts"; "version" ]))
          || not (Poly.equal (member "version" json) (`Int 1))
        then failwith "account configuration requires version 1 and accounts";
        member "accounts" json
        |> to_list
        |> List.map ~f:(fun json ->
          let fields = to_assoc json in
          let names = List.map fields ~f:fst in
          let required = [ "institution"; "bank_id"; "account_id"; "account_type" ] in
          let allowed = required @ [ "ledger_account"; "iban"; "settlement_account" ] in
          if
            List.contains_dup names ~compare:String.compare
            || (not
                  (List.for_all required ~f:(fun k ->
                     List.mem names k ~equal:String.equal)))
            || List.exists names ~f:(fun k ->
              not (List.mem allowed k ~equal:String.equal))
          then failwith "unknown, missing or duplicated account configuration field";
          let r =
            reference_of_json
              (`Assoc
                  (List.filter fields ~f:(fun (k, _) ->
                     List.mem required k ~equal:String.equal)))
            |> Or_error.ok_exn
          in
          let ledger_account =
            List.Assoc.find fields "ledger_account" ~equal:String.equal
            |> Option.map ~f:(fun v ->
              let name = to_string v in
              Bank_account_mapping.create
                ~institution:r.institution
                ~bank_id:r.bank_id
                ~account_id:r.account_id
                ~account_type:r.account_type
                ~ledger_account:name
              |> Or_error.ok_exn
              |> fun (a : Bank_account_mapping.t) -> a.ledger_account)
          in
          let update name parse =
            match List.Assoc.find fields name ~equal:String.equal with
            | None -> Keep
            | Some `Null -> Clear
            | Some v -> Set (parse v)
          in
          { reference = r
          ; ledger_account
          ; iban =
              update "iban" (fun v -> Iban.of_string (to_string v) |> Or_error.ok_exn)
          ; settlement_account =
              update "settlement_account" (fun v ->
                reference_of_json v |> Or_error.ok_exn)
          })
      | _ -> failwith "expected versioned account configuration"
    in
    if
      List.contains_dup
        (List.map rows ~f:(fun a -> a.reference.institution, source_account a.reference))
        ~compare:[%compare: string * string]
    then failwith "duplicate configured account";
    rows)
;;

let to_json rows =
  let update key convert = function
    | Keep -> []
    | Clear -> [ key, `Null ]
    | Set value -> [ key, convert value ]
  in
  Yojson.Safe.pretty_to_string
    (`Assoc
        [ "version", `Int 1
        ; ( "accounts"
          , `List
              (List.map rows ~f:(fun row ->
                 `Assoc
                   (reference_fields row.reference
                    @ Option.value_map row.ledger_account ~default:[] ~f:(fun a ->
                      [ "ledger_account", `String a ])
                    @ update "iban" (fun iban -> `String (Iban.to_string iban)) row.iban
                    @ update "settlement_account" reference_to_json row.settlement_account
                   ))) )
        ])
;;
