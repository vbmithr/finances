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

(** The text a rule target may contain where the value its expression matched
    goes, so that [wise:{value}] keeps a provider code as a provider-specific type. *)
val placeholder : string

val has_placeholder : t -> bool

(** Replace the placeholder in a provider-specific type; other types are unchanged. *)
val expand : t -> value:string -> t
