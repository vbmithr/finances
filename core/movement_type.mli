(** A provider-reported movement kind. [Other] retains a provider-qualified
    value when no common kind applies. This is not an accounting category or
    a confirmed transfer between owned accounts. *)
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

val to_string : t -> string
val of_string : string -> t
val common : (t * string) list
