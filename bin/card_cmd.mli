open Base

(** Cards and the card accounts they belong to. *)

(** A card as [fin card list --json] prints it. *)
val card_json : Finances_store.Cards.t Jsont.t

val cmd : unit Cmdliner.Cmd.t
