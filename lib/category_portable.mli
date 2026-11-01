open Base

type decision =
  { institution : string
  ; source_account : string
  ; kind : string
  ; source_id : string
  ; occurred : string
  ; payload_sha256 : string
  ; fields_sha256 : string
  ; account : string
  ; reason : string
  }

type t =
  { categories : string list
  ; rules : Classification_rule.t list
  ; decisions : decision list
  }

val codec : t Jsont.t
