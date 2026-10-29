type t =
  | Card
  | Transfer
  | Conversion
  | Direct_debit
  | Deposit
  | Withdrawal
  | Other of string

let to_string = function
  | Card -> "card"
  | Transfer -> "transfer"
  | Conversion -> "conversion"
  | Direct_debit -> "direct-debit"
  | Deposit -> "deposit"
  | Withdrawal -> "withdrawal"
  | Other value -> value
;;

let of_string = function
  | "card" -> Card
  | "transfer" -> Transfer
  | "conversion" -> Conversion
  | "direct-debit" -> Direct_debit
  | "deposit" -> Deposit
  | "withdrawal" -> Withdrawal
  | value -> Other value
;;
