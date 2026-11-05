open Base

type decision =
  { institution : string
  ; source_account : string
  ; kind : string
  ; source_id : string
  ; occurred : string
  ; payload_sha256 : string
  ; fields_sha256 : string
  ; movement_role : string
  ; account : string option
  ; reason : string option
  ; type_override : string option
  }

type resolution =
  { source : decision
  ; source_quantity : Quantity.t
  ; source_commodity : Commodity.t
  ; account : string
  ; commodity : Commodity.t
  ; symbol : string
  }

type t =
  { categories : string list
  ; rules : Classification_rule.t list
  ; decisions : decision list
  ; resolutions : resolution list
  }

val decision : decision Jsont.t
val rule : Classification_rule.t Jsont.t
val categories_codec : t Jsont.t
val rules_codec : t Jsont.t
val decisions_codec : t Jsont.t
