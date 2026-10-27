open Base

let key = "bank-unclassified-v1"

let account_component value =
  if
    Result.is_ok (Validation.token "account component" value)
    && not (String.is_prefix value ~prefix:"x-")
  then value
  else
    "x-"
    ^ (String.to_list value
       |> List.map ~f:(fun c -> Printf.sprintf "%02x" (Char.to_int c))
       |> String.concat)
;;

let ledger_account_for ~institution (account : Bank_transaction.account) =
  let name =
    String.concat
      ~sep:":"
      ([ "assets"; "bank" ]
       @ List.map
           [ institution; account.bank_id; account.account_type; account.account_id ]
           ~f:account_component)
  in
  Validation.account name
;;

let ledger_account ~institution (account : Bank_ofx.account) =
  ledger_account_for
    ~institution
    { Bank_transaction.bank_id = account.bank_id
    ; account_id = account.account_id
    ; account_type = account.account_type
    ; currency = account.currency
    }
;;

let description_component text =
  text
  |> String.to_list
  |> List.concat_map ~f:(fun c ->
    if Char.to_int c < 32 || Char.equal c '\127' then [ ' ' ] else [ c ])
  |> String.of_char_list
  |> fun s ->
  String.substr_replace_all s ~pattern:";" ~with_:"；"
  |> fun s ->
  String.substr_replace_all s ~pattern:"|" ~with_:"｜"
  |> String.split ~on:' '
  |> List.filter ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:" "
;;

let entry ?bank_account ?(counter_account = "equity:unclassified") event =
  let open Or_error.Let_syntax in
  let%bind transaction = Bank_transaction.of_event event in
  let account = transaction.account in
  let%bind bank_account =
    match bank_account with
    | None -> ledger_account_for ~institution:event.venue account
    | Some name -> Validation.account name
  in
  let%bind incoming =
    Accounting.posting
      ~account:bank_account
      ~commodity:account.currency
      transaction.amount
  in
  let%bind outgoing =
    Accounting.posting
      ~account:counter_account
      ~commodity:account.currency
      (Quantity.neg Quantity.(transaction.amount + transaction.fee))
  in
  let descriptions =
    List.filter_opt [ transaction.name; transaction.memo ]
    |> List.map ~f:description_component
    |> List.filter ~f:(Fn.non String.is_empty)
  in
  let description =
    match descriptions with
    | [] -> "Bank transaction"
    | values -> String.concat ~sep:" | " values
  in
  let%bind fees =
    if Quantity.is_zero transaction.fee
    then Ok []
    else (
      let%map fee =
        Accounting.posting
          ~account:"expenses:bank-fees"
          ~commodity:account.currency
          transaction.fee
      in
      [ fee ])
  in
  Accounting.entry ~date:transaction.booked_on ~description ([ incoming; outgoing ] @ fees)
;;
