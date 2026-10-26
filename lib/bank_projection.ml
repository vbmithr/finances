open Core

let key = "bank-unclassified-v1"

let account_component value =
  if
    Result.is_ok (Validation.token "account component" value)
    && not (String.is_prefix value ~prefix:"x-")
  then value
  else
    "x-"
    ^ (String.to_list value
       |> List.map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
       |> String.concat)
;;

let ledger_account ~institution (account : Bank_ofx.account) =
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

let entry event =
  let open Or_error.Let_syntax in
  let%bind account, transaction = Bank_ofx.source_transaction event in
  let%bind bank_account = ledger_account ~institution:event.venue account in
  let%bind incoming =
    Accounting.posting
      ~account:bank_account
      ~commodity:account.currency
      transaction.amount
  in
  let%bind outgoing =
    Accounting.posting
      ~account:"equity:unclassified"
      ~commodity:account.currency
      (Bigdecimal.neg transaction.amount)
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
  Accounting.entry ~date:transaction.booked_on ~description [ incoming; outgoing ]
;;
