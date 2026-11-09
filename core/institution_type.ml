open Base

type network =
  | Evm of { chain_id : int }
  | Other of string
  | Unspecified
[@@deriving compare, equal]

type t =
  | Bank
  | Crypto_exchange
  | Wallet of network
[@@deriving compare, equal]

let to_string = function
  | Bank -> "bank"
  | Crypto_exchange -> "crypto-exchange"
  | Wallet _ -> "wallet"
;;

let label = function
  | Bank -> "bank"
  | Crypto_exchange -> "crypto exchange"
  | Wallet _ -> "wallet"
;;

let kind_jsont =
  Jsont.map
    Jsont.string
    ~dec:(function
      | "bank" -> Bank
      | "crypto-exchange" -> Crypto_exchange
      | "wallet" -> Wallet Unspecified
      | value -> Jsont.Error.msgf Jsont.Meta.none "unknown institution type %S" value)
    ~enc:to_string
;;
