open Base

type t =
  { contents : string
  ; account_id : string
  ; currency : Iso4217.t
  ; opening_on : Date.t
  ; closing_on : Date.t
  ; opening : Quantity.t
  ; closing : Quantity.t
  ; transactions : Mt940_transaction.t list
  }

let recognizes (profile : Mt940_profile.t) s =
  let p = String.prefix s 4096 in
  String.is_prefix p ~prefix:profile.recognizer_prefix
  && String.is_substring p ~substring:":60F:"
;;

let parse (engine : Mt940_transaction.engine) contents =
  let profile = Mt940_transaction.profile engine in
  Or_error.try_with (fun () ->
    if not (Stdlib.String.is_valid_utf_8 contents)
    then failwith ("expected UTF-8 " ^ profile.name ^ " MT940");
    if not (recognizes profile contents)
    then failwith ("expected " ^ profile.name ^ " MT940");
    let reader = Mt940_dialect.make profile in
    let statement =
      match Mt940_grammar.parse contents |> Or_error.ok_exn with
      | [ statement ] -> statement
      | _ -> failwith ("expected one " ^ profile.name ^ " MT940 statement")
    in
    let account_id = Mt940_dialect.account_id reader statement in
    ignore
      (Validation.token ("" ^ profile.name ^ " account") account_id |> Or_error.ok_exn
       : string);
    let currency = Mt940_dialect.currency reader statement in
    let code = Iso4217.to_alpha_code currency in
    let opening_on = Mt940_dialect.opening_on reader statement
    and closing_on = Mt940_dialect.closing_on reader statement
    and opening = Mt940_dialect.opening reader statement
    and closing = Mt940_dialect.closing reader statement in
    let balance_currency = function
      | Some (balance : Mt940_grammar.balance) -> Iso4217.equal balance.currency currency
      | None -> false
    in
    if not (balance_currency statement.opening && balance_currency statement.closing)
    then failwith ("inconsistent " ^ profile.name ^ " balance currency");
    let rec validate_fields = function
      | [] -> ()
      | ({ tag = "61"; _ } : Mt940_grammar.field) :: { tag = "86"; _ } :: rest ->
        validate_fields rest
      | { tag; _ } :: rest
        when List.mem [ "20"; "25"; "28"; "60F"; "62F" ] tag ~equal:String.equal ->
        validate_fields rest
      | _ ->
        failwith ("unsupported " ^ profile.name ^ " MT940 field or unpaired transaction")
    in
    validate_fields statement.fields;
    if Date.(opening_on > closing_on)
    then failwith ("inconsistent " ^ profile.name ^ " statement balance interval");
    let transactions = Mt940_dialect.entries reader statement |> Or_error.ok_exn in
    List.iter transactions ~f:(fun (tx : Mt940_transaction.t) ->
      if Date.(tx.booked_on < opening_on || tx.booked_on > closing_on)
      then failwith ("" ^ profile.name ^ " entry outside statement interval");
      if not (String.equal (Iso4217.to_alpha_code tx.currency) code)
      then failwith ("" ^ profile.name ^ " entry in another currency");
      if not (String.equal tx.account_id account_id)
      then failwith ("" ^ profile.name ^ " entry for another account"));
    let movements =
      List.fold transactions ~init:Quantity.zero ~f:(fun total tx ->
        Quantity.(total + tx.Mt940_transaction.amount))
    in
    if not Quantity.(opening + movements = closing)
    then
      failwith
        (""
         ^ profile.name
         ^ " opening balance plus movements does not equal closing balance");
    { contents
    ; account_id
    ; currency
    ; opening_on
    ; closing_on
    ; opening
    ; closing
    ; transactions
    })
  |> fun result -> Or_error.tag result ~tag:("" ^ profile.name ^ " MT940")
;;
