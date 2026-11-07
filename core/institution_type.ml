open Base

type t =
  | Bank
  | Crypto_exchange
  | Wallet
[@@deriving compare, equal]

let to_string = function
  | Bank -> "bank"
  | Crypto_exchange -> "crypto-exchange"
  | Wallet -> "wallet"
;;

let label = function
  | Bank -> "bank"
  | Crypto_exchange -> "crypto exchange"
  | Wallet -> "wallet"
;;

let jsont =
  Jsont.enum [ "bank", Bank; "crypto-exchange", Crypto_exchange; "wallet", Wallet ]
;;
