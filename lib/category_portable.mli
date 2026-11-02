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

type t =
  { categories : string list
  ; rules : Classification_rule.t list
  ; decisions : decision list
  }

(** Version 1 combined backup, retained for restoring existing exports. *)
val codec : t Jsont.t

val categories_codec : t Jsont.t
val rules_codec : t Jsont.t
val decisions_codec : t Jsont.t
val legacy_decisions_codec : t Jsont.t
val previous_decisions_codec : t Jsont.t
