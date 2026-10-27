open Base

type t = Decimal.t

let finite_exn value =
  if not (Decimal.is_finite value) then invalid_arg "quantity must be finite";
  value
;;

let of_string text = finite_exn (Decimal.of_string text)
let zero = Decimal.zero
let one = Decimal.one
let equal = Decimal.equal
let compare = Decimal.compare
let is_zero value = equal value zero
let neg value = Decimal.copy_negate (finite_exn value)

let to_string value =
  let value = finite_exn value in
  if is_zero value
  then "0"
  else (
    match String.lsplit2 (Decimal.to_string ~format:`plain value) ~on:'.' with
    | None -> Decimal.to_string ~format:`plain value
    | Some (whole, fraction) ->
      let fraction = String.rstrip fraction ~drop:(Char.equal '0') in
      if String.is_empty fraction then whole else whole ^ "." ^ fraction)
;;

let exponent value =
  let _, coefficient, exponent = Decimal.to_tuple (finite_exn value) in
  if is_zero value
  then 0
  else
    exponent
    + String.length coefficient
    - String.length (String.rstrip coefficient ~drop:(Char.equal '0'))
;;

let sexp_of_t value = Sexp.Atom (to_string value)
let t_of_sexp sexp = of_string (String.t_of_sexp sexp)

(* Accounting sums and products must be exact even if a caller has selected a
   small default decimal context. Derive sufficient precision from the operands
   and trap inexact results. Decimal performs all arithmetic; we do not mutate
   its global context or implement another arithmetic engine. *)
let checked_add a b = Int64.(to_int_exn (of_int a + of_int b))
let checked_sub a b = Int64.(to_int_exn (of_int a - of_int b))

let parts value =
  let _, coefficient, exponent = Decimal.to_tuple (finite_exn value) in
  String.length coefficient, exponent
;;

let context ~precision ~lowest ~highest =
  let context =
    Decimal.Context.make
      ~prec:precision
      ~e_min:(Int.min (-999999) lowest)
      ~e_max:(Int.max 999999 highest)
      ()
  in
  Decimal.Signal.set (Decimal.Context.traps context) Decimal.Signal.inexact true;
  context
;;

let sum operation a b =
  let digits_a, exponent_a = parts a
  and digits_b, exponent_b = parts b in
  let lowest = Int.min exponent_a exponent_b in
  let highest =
    checked_add
      1
      (Int.max (checked_add exponent_a digits_a) (checked_add exponent_b digits_b))
  in
  let precision = checked_sub highest lowest in
  finite_exn (operation ~context:(context ~precision ~lowest ~highest) a b)
;;

let add a b = sum (fun ~context -> Decimal.add ~context) a b
let sub a b = sum (fun ~context -> Decimal.sub ~context) a b

let mul a b =
  let digits_a, exponent_a = parts a
  and digits_b, exponent_b = parts b in
  let precision = checked_add digits_a digits_b in
  let lowest = checked_add exponent_a exponent_b in
  let highest = checked_add lowest precision in
  finite_exn (Decimal.mul ~context:(context ~precision ~lowest ~highest) a b)
;;

let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( = ) = equal
let ( <> ) a b = not (equal a b)
let ( < ) a b = compare a b < 0
let ( <= ) a b = compare a b <= 0
let ( > ) a b = compare a b > 0
let ( >= ) a b = compare a b >= 0
