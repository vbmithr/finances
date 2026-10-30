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
  | value -> Other value
;;
