open Base

type t =
  | Bank
  | Crypto_exchange
[@@deriving compare, equal]

let to_string = function
  | Bank -> "bank"
  | Crypto_exchange -> "crypto-exchange"
;;

let label = function
  | Bank -> "bank"
  | Crypto_exchange -> "crypto exchange"
;;

let jsont = Jsont.enum [ "bank", Bank; "crypto-exchange", Crypto_exchange ]
