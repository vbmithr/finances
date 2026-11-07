open Base

type settlement =
  { source : Category_portable.decision
  ; card_institution : string
  ; card_source_account : string
  ; ledger_account : string
  ; reason : string
  }

type reversal =
  { original : Category_portable.decision
  ; reversal : Category_portable.decision
  }

(** A card number assigned to a card account; see docs/cards.md. *)
type card =
  { institution : string
  ; card_reference : string
  ; card_source_account : string
  }

(** What a provider's symbol is, see docs/operations.md: [None] is ignored. *)
type asset =
  { provider : string
  ; symbol : string
  ; commodity : Commodity.t option
  }

type t =
  { accounts : Bank_account_config.t list option
  ; assets : asset list option
  ; cards : card list option
  ; categories : string list option
  ; rules : Classification_rule.t list option
  ; assignments : Category_portable.t option
  ; settlements : settlement list option
  ; reversals : reversal list option
  ; exchanges : Exchange_portable.t option
  ; transfers : Transfer_portable.t option
  }

let movement = Category_portable.decision

let settlement =
  Jsont.Object.map
    (fun source card_institution card_source_account ledger_account reason : settlement ->
       { source; card_institution; card_source_account; ledger_account; reason })
  |> Jsont.Object.mem "source" movement ~enc:(fun (r : settlement) -> r.source)
  |> Jsont.Object.mem "card_institution" Jsont.string ~enc:(fun (r : settlement) ->
    r.card_institution)
  |> Jsont.Object.mem "card_source_account" Jsont.string ~enc:(fun (r : settlement) ->
    r.card_source_account)
  |> Jsont.Object.mem "ledger_account" Jsont.string ~enc:(fun r -> r.ledger_account)
  |> Jsont.Object.mem "reason" Jsont.string ~enc:(fun (r : settlement) -> r.reason)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let reversal =
  Jsont.Object.map (fun original reversal : reversal -> { original; reversal })
  |> Jsont.Object.mem "original" movement ~enc:(fun r -> r.original)
  |> Jsont.Object.mem "reversal" movement ~enc:(fun r -> r.reversal)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let card =
  Jsont.Object.map (fun institution card_reference card_source_account : card ->
    { institution; card_reference; card_source_account })
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun (c : card) -> c.institution)
  |> Jsont.Object.mem "card_reference" Jsont.string ~enc:(fun c -> c.card_reference)
  |> Jsont.Object.mem "card_source_account" Jsont.string ~enc:(fun c ->
    c.card_source_account)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let asset =
  Jsont.Object.map (fun provider symbol commodity : asset ->
    { provider; symbol; commodity })
  |> Jsont.Object.mem "provider" Jsont.string ~enc:(fun (a : asset) -> a.provider)
  |> Jsont.Object.mem "symbol" Jsont.string ~enc:(fun (a : asset) -> a.symbol)
  |> Jsont.Object.mem "commodity" (Jsont.option Commodity.codec) ~enc:(fun (a : asset) ->
    a.commodity)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let codec =
  let section name codec enc map = Jsont.Object.opt_mem name codec ~enc map in
  Jsont.Object.map
    (fun
        ()
         ()
         accounts
         assets
         cards
         categories
         rules
         assignments
         settlements
         reversals
         exchanges
         transfers
       ->
       { accounts
       ; assets
       ; cards
       ; categories
       ; rules
       ; assignments
       ; settlements
       ; reversals
       ; exchanges
       ; transfers
       })
  |> Jsont.Object.mem "format" (Json.tag "fin-decisions") ~enc:(fun _ -> ())
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> section "accounts" Bank_account_config.snapshot_jsont (fun r -> r.accounts)
  |> section "assets" (Jsont.list asset) (fun r -> r.assets)
  |> section "cards" (Jsont.list card) (fun r -> r.cards)
  |> section "categories" (Jsont.list Jsont.string) (fun r -> r.categories)
  |> section "rules" (Jsont.list Category_portable.rule) (fun r -> r.rules)
  |> section "assignments" Category_portable.decisions_codec (fun r -> r.assignments)
  |> section "settlements" (Jsont.list settlement) (fun r -> r.settlements)
  |> section "reversals" (Jsont.list reversal) (fun r -> r.reversals)
  |> section "exchanges" Exchange_portable.codec (fun r -> r.exchanges)
  |> section "transfers" Transfer_portable.codec (fun r -> r.transfers)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;
