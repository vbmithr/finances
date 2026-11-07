open Base

type t =
  | Fiat of Iso4217.t
  | Asset of string
  | Reported of
      { provider : string
      ; symbol : string
      }

let fiat currency = Fiat currency

let asset symbol =
  let open Or_error.Let_syntax in
  let%bind symbol = Validation.token "asset" symbol in
  if Option.is_some (Iso4217.of_alpha_code (String.uppercase symbol))
  then Or_error.errorf "asset %s is an ISO 4217 currency code, which names fiat" symbol
  else Ok (Asset symbol)
;;

let reported ~provider ~symbol =
  let open Or_error.Let_syntax in
  let%bind provider = Validation.token "commodity provider" provider in
  let%map symbol = Validation.token "commodity symbol" symbol in
  Reported { provider; symbol }
;;

let provider = function
  | Fiat _ | Asset _ -> None
  | Reported { provider; _ } -> Some provider
;;

let symbol = function
  | Fiat currency -> Some (Iso4217.to_alpha_code currency)
  | Asset symbol | Reported { symbol; _ } -> Some symbol
;;

let journal_symbol = function
  | Fiat currency -> Some (Iso4217.to_alpha_code currency)
  | Asset symbol -> Some symbol
  | Reported _ -> None
;;

let display = function
  | Fiat currency -> Iso4217.to_alpha_code currency
  | Asset symbol | Reported { symbol; _ } -> symbol
;;

let equal a b =
  match a, b with
  | Fiat a, Fiat b -> Iso4217.equal a b
  | Asset a, Asset b -> String.equal a b
  | Reported a, Reported b ->
    String.equal a.provider b.provider && String.equal a.symbol b.symbol
  | _ -> false
;;

let codec =
  Jsont.Object.map (fun kind code provider ->
    let open Or_error.Let_syntax in
    let result =
      match kind, code, provider with
      | "fiat", Some code, None ->
        let%map currency = Validation.currency "commodity" code in
        Fiat currency
      | "asset", Some code, None -> asset code
      | "reported", Some symbol, Some provider -> reported ~provider ~symbol
      | _ -> Or_error.error_string "invalid commodity fields"
    in
    Json.or_error result)
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(function
    | Fiat _ -> "fiat"
    | Asset _ -> "asset"
    | Reported _ -> "reported")
  |> Jsont.Object.opt_mem "code" Jsont.string ~enc:symbol
  |> Jsont.Object.opt_mem "provider" Jsont.string ~enc:provider
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;
