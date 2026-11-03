open Base

let exchange ~date ~description movements =
  let open Or_error.Let_syntax in
  match movements with
  | [ (first_account, first_commodity, first_symbol, first_quantity)
    ; (second_account, second_commodity, second_symbol, second_quantity)
    ] ->
    if Commodity.equal first_commodity second_commodity
    then Or_error.error_string "exchange requires two distinct commodities"
    else if
      Quantity.is_zero first_quantity
      || Quantity.is_zero second_quantity
      || Bool.equal
           (Quantity.compare first_quantity Quantity.zero > 0)
           (Quantity.compare second_quantity Quantity.zero > 0)
    then Or_error.error_string "exchange requires one positive and one negative movement"
    else (
      let%bind () =
        match first_commodity, second_commodity with
        | Commodity.Reported _, _ | _, Commodity.Reported _ ->
          Or_error.error_string
            "provider-reported commodity must be resolved before accounting"
        | _ -> Ok ()
      in
      if String.equal first_symbol second_symbol
      then Or_error.error_string "distinct commodities have the same accounting symbol"
      else (
        let%bind first =
          Accounting.posting ~account:first_account ~commodity:first_symbol first_quantity
        in
        let%bind second =
          Accounting.posting
            ~account:second_account
            ~commodity:second_symbol
            second_quantity
        in
        let%bind first_trading =
          Accounting.posting
            ~account:"equity:trading"
            ~commodity:first_symbol
            (Quantity.neg first_quantity)
        in
        let%bind second_trading =
          Accounting.posting
            ~account:"equity:trading"
            ~commodity:second_symbol
            (Quantity.neg second_quantity)
        in
        Accounting.entry
          ~date
          ~description
          [ first; second; first_trading; second_trading ]))
  | _ -> Or_error.error_string "exchange requires exactly two movements"
;;

let transfer ~date ~description movements =
  let open Or_error.Let_syntax in
  match movements with
  | [ (first_account, first_commodity, first_symbol, first_quantity)
    ; (second_account, second_commodity, second_symbol, second_quantity)
    ] ->
    if String.equal first_account second_account
    then Or_error.error_string "transfer requires distinct accounts"
    else if
      (not (Commodity.equal first_commodity second_commodity))
      || not (String.equal first_symbol second_symbol)
    then
      Or_error.error_string
        "transfer requires the same canonical commodity and journal symbol"
    else if
      match first_commodity with
      | Commodity.Reported _ -> true
      | _ -> false
    then Or_error.error_string "transfer commodity must be resolved"
    else if
      Quantity.is_zero first_quantity
      || not (Quantity.equal first_quantity (Quantity.neg second_quantity))
    then Or_error.error_string "transfer requires equal and opposite nonzero quantities"
    else (
      let%bind first =
        Accounting.posting ~account:first_account ~commodity:first_symbol first_quantity
      in
      let%bind second =
        Accounting.posting
          ~account:second_account
          ~commodity:second_symbol
          second_quantity
      in
      Accounting.entry ~date ~description [ first; second ])
  | _ -> Or_error.error_string "transfer requires exactly two movements"
;;
