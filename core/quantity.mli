open Base

(** Finite decimal amounts. The representation is the decimal library's type;
    sums, differences and products use operand-sized contexts and trap inexact
    results, independently of [Decimal.Context.default]. *)
type t = Decimal.t [@@deriving sexp, compare, equal]

val finite_exn : t -> t
val of_string : string -> t
val to_string : t -> string
val zero : t
val one : t
val exponent : t -> int
val is_zero : t -> bool
val neg : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( = ) : t -> t -> bool
val ( <> ) : t -> t -> bool
val ( < ) : t -> t -> bool
val ( <= ) : t -> t -> bool
val ( > ) : t -> t -> bool
val ( >= ) : t -> t -> bool
