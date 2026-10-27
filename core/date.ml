open Base

type t = Ptime.date

let instant_exn date =
  match Ptime.of_date date with
  | Some value -> value
  | None -> invalid_arg "invalid calendar date"
;;

let validate_exn date = ignore (instant_exn date : Ptime.t)

let of_string text =
  match String.split text ~on:'-' with
  | [ year; month; day ] ->
    let date = Int.of_string year, Int.of_string month, Int.of_string day in
    validate_exn date;
    date
  | _ -> invalid_arg "expected YYYY-MM-DD"
;;

let to_string ((year, month, day) as date) =
  validate_exn date;
  Printf.sprintf "%04d-%02d-%02d" year month day
;;

let year (year, _, _) = year
let compare = Poly.compare
let equal = Poly.equal
let sexp_of_t date = Sexp.Atom (to_string date)
let t_of_sexp sexp = of_string (String.t_of_sexp sexp)

let diff a b =
  (* UTC midnight is only an intermediate for calendar-day arithmetic, never
     persisted as the occurrence time of a date-only bank transaction. *)
  Ptime.diff (instant_exn a) (instant_exn b) |> Ptime.Span.to_d_ps |> fst
;;

let ( < ) a b = compare a b < 0
let ( <= ) a b = compare a b <= 0
let ( > ) a b = compare a b > 0
let ( >= ) a b = compare a b >= 0
