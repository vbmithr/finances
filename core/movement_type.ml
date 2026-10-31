open Base

type t =
  | Card
  | Card_settlement
  | Transfer
  | Conversion
  | Direct_debit
  | Deposit
  | Withdrawal
  | Interest
  | Fee
  | Cheque
  | Bonus
  | Other of string

let to_string = function
  | Card -> "card"
  | Card_settlement -> "card-settlement"
  | Transfer -> "transfer"
  | Conversion -> "conversion"
  | Direct_debit -> "direct-debit"
  | Deposit -> "deposit"
  | Withdrawal -> "withdrawal"
  | Interest -> "interest"
  | Fee -> "fee"
  | Cheque -> "cheque"
  | Bonus -> "bonus"
  | Other value -> value
;;

let of_string = function
  | "card" -> Card
  | "card-settlement" -> Card_settlement
  | "transfer" -> Transfer
  | "conversion" -> Conversion
  | "direct-debit" -> Direct_debit
  | "deposit" -> Deposit
  | "withdrawal" -> Withdrawal
  | "interest" -> Interest
  | "fee" -> Fee
  | "cheque" -> Cheque
  | "bonus" -> Bonus
  | value -> Other value
;;

let common =
  [ Card, "Card purchase or refund"
  ; Card_settlement, "Credit card settlement"
  ; Transfer, "Transfer reported by the provider"
  ; Conversion, "Currency conversion"
  ; Direct_debit, "Direct debit"
  ; Deposit, "Deposit"
  ; Withdrawal, "Cash withdrawal or reversal"
  ; Interest, "Interest credited or charged"
  ; Fee, "Fee or fee refund"
  ; Cheque, "Cheque issued"
  ; Bonus, "Bonus or promotional credit"
  ]
;;

let placeholder = "{value}"

let has_placeholder = function
  | Other value -> String.is_substring value ~substring:placeholder
  | _ -> false
;;

let expand t ~value =
  match t with
  | Other text when String.is_substring text ~substring:placeholder ->
    Other (String.substr_replace_all text ~pattern:placeholder ~with_:value)
  | t -> t
;;
