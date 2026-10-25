open Core

type t =
  { venue : string
  ; market : string
  ; base : string
  ; quote : string
  ; settlement : string
  ; multiplier : Bigdecimal.t
  }

let linear_perpetual ~venue ~market ~base ~quote ~settlement ~multiplier =
  let open Or_error.Let_syntax in
  let%bind venue = Validation.text "venue" venue in
  let%bind market = Validation.text "market" market in
  let%bind base = Validation.token "base commodity" base in
  let%bind quote = Validation.token "quote commodity" quote in
  let%bind settlement = Validation.token "settlement commodity" settlement in
  if Bigdecimal.(multiplier <= zero)
  then Or_error.error_string "multiplier must be positive"
  else Ok { venue; market; base; quote; settlement; multiplier }
;;
