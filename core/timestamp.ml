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

let to_rfc3339 time = Ptime.to_rfc3339 ~frac_s:12 ~tz_offset_s:0 time

let of_unix_ms_exn ms =
  let day_ms = 86400000L in
  let days = Stdlib.Int64.div ms day_ms
  and remainder = Stdlib.Int64.rem ms day_ms in
  let days, remainder =
    if Int64.(remainder < 0L)
    then Int64.(days - 1L), Int64.(remainder + day_ms)
    else days, remainder
  in
  let span = Ptime.Span.v (Int64.to_int_exn days, Int64.(remainder * 1000000000L)) in
  match Ptime.of_span span with
  | Some time -> time
  | None -> invalid_arg "Unix milliseconds outside Ptime range"
;;
