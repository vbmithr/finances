open Base

let of_rfc3339_exn text =
  match Ptime.of_rfc3339 ~strict:true text with
  | Ok (time, _, _) -> time
  | Error _ -> invalid_arg "invalid RFC 3339 timestamp"
;;

let nanoseconds_since_epoch_exn time =
  let days, picoseconds = Ptime.to_span time |> Ptime.Span.to_d_ps in
  if days < 0 then invalid_arg "timestamp is before the supported epoch";
  let ns_per_day = 86400000000000L in
  let days = Int64.of_int days in
  let fraction = Int64.(picoseconds / 1000L) in
  if Int64.(days > (max_value - fraction) / ns_per_day)
  then invalid_arg "timestamp exceeds supported nanosecond range";
  Int64.((days * ns_per_day) + fraction)
;;
