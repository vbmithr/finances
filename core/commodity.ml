open Base

type t =
  | Fiat of Iso4217.t
  | Onchain_native of { network : string }
  | Onchain_token of
      { network : string
      ; contract : string
      ; token_id : string option
      }
  | Reported of
      { provider : string
      ; symbol : string
      }

let fiat currency = Fiat currency

let onchain_native ~network =
  Result.map (Validation.token "network" network) ~f:(fun network ->
    Onchain_native { network })
;;

let onchain_token ~network ~contract ~token_id =
  let open Or_error.Let_syntax in
  let%bind network = Validation.token "network" network in
  let%bind contract = Validation.text "token contract" contract in
  let%map token_id =
    Option.map token_id ~f:(Validation.text "token ID")
    |> Option.value_map ~default:(Ok None) ~f:(Result.map ~f:Option.some)
  in
  Onchain_token { network; contract; token_id }
;;

let reported ~provider ~symbol =
  let open Or_error.Let_syntax in
  let%bind provider = Validation.token "commodity provider" provider in
  let%map symbol = Validation.token "commodity symbol" symbol in
  Reported { provider; symbol }
;;

let provider = function
  | Fiat _ | Onchain_native _ | Onchain_token _ -> None
  | Reported { provider; _ } -> Some provider
;;

let symbol = function
  | Fiat currency -> Some (Iso4217.to_alpha_code currency)
  | Reported { symbol; _ } -> Some symbol
  | Onchain_native _ | Onchain_token _ -> None
;;

let display = function
  | Fiat currency -> Iso4217.to_alpha_code currency
  | Reported { symbol; _ } -> symbol
  | Onchain_native { network } -> network ^ ":native"
  | Onchain_token { network; contract; token_id } ->
    network ^ ":" ^ contract ^ Option.value_map token_id ~default:"" ~f:(( ^ ) ":")
;;

let equal a b =
  match a, b with
  | Fiat a, Fiat b -> Iso4217.equal a b
  | Onchain_native a, Onchain_native b -> String.equal a.network b.network
  | Onchain_token a, Onchain_token b ->
    String.equal a.network b.network
    && String.equal a.contract b.contract
    && Option.equal String.equal a.token_id b.token_id
  | Reported a, Reported b ->
    String.equal a.provider b.provider && String.equal a.symbol b.symbol
  | _ -> false
;;

let codec =
  Jsont.Object.map (fun kind code network contract token_id provider ->
    let open Or_error.Let_syntax in
    let result =
      match kind, code, network, contract, token_id, provider with
      | "fiat", Some code, None, None, None, None ->
        let%map currency = Validation.currency "commodity" code in
        Fiat currency
      | "onchain-native", None, Some network, None, None, None -> onchain_native ~network
      | "onchain-token", None, Some network, Some contract, token_id, None ->
        onchain_token ~network ~contract ~token_id
      | "reported", Some symbol, None, None, None, Some provider ->
        reported ~provider ~symbol
      | _ -> Or_error.error_string "invalid commodity fields"
    in
    Json.or_error result)
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(function
    | Fiat _ -> "fiat"
    | Onchain_native _ -> "onchain-native"
    | Onchain_token _ -> "onchain-token"
    | Reported _ -> "reported")
  |> Jsont.Object.opt_mem "code" Jsont.string ~enc:symbol
  |> Jsont.Object.opt_mem "network" Jsont.string ~enc:(function
    | Onchain_native { network } | Onchain_token { network; _ } -> Some network
    | _ -> None)
  |> Jsont.Object.opt_mem "contract" Jsont.string ~enc:(function
    | Onchain_token { contract; _ } -> Some contract
    | _ -> None)
  |> Jsont.Object.opt_mem "token_id" Jsont.string ~enc:(function
    | Onchain_token { token_id; _ } -> token_id
    | _ -> None)
  |> Jsont.Object.opt_mem "provider" Jsont.string ~enc:provider
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;
