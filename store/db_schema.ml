open Base
open Rel

(** One current experimental baseline; older databases must be reimported. Banks
    have no tables of their own. *)
let version = 38

let timestamp = `Expr "(strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')"

let decode_exn f text =
  match Or_error.try_with (fun () -> f text) with
  | Ok value -> value
  | Error error -> failwith (Error.to_string_hum error)
;;

let instant =
  Type.coded
    (Type.Coded.make
       ~name:"instant"
       ~equal:Ptime.equal
       Type.text
       ~enc:Timestamp.to_rfc3339
       ~dec:(decode_exn Timestamp.of_rfc3339_exn))
;;

let statement_time =
  Type.coded
    (Type.Coded.make
       ~name:"statement_time"
       Type.text
       ~enc:Temporal.to_string
       ~dec:(decode_exn Temporal.of_string_exn))
;;

module Source_events = struct
  type t =
    { id : int64
    ; venue : string
    ; account : string
    ; kind : string
    ; source_id : string
    ; occurred_at : Ptime.t option
    ; occurred_on : string option
    ; retrieved_at : Ptime.t
    ; payload : string
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let venue = Col.make "venue" Type.text (fun (r : t) -> r.venue)
  let account = Col.make "account" Type.text (fun (r : t) -> r.account)
  let kind = Col.make "kind" Type.text (fun (r : t) -> r.kind)
  let source_id = Col.make "source_id" Type.text (fun (r : t) -> r.source_id)

  let occurred_at =
    Col.make "occurred_at" Type.(option instant) (fun (r : t) -> r.occurred_at)
  ;;

  let occurred_on =
    Col.make "occurred_on" Type.(option text) (fun (r : t) -> r.occurred_on)
  ;;

  let retrieved_at =
    Col.make ~default:timestamp "retrieved_at" instant (fun (r : t) -> r.retrieved_at)
  ;;

  let payload = Col.make "payload" Type.blob (fun (r : t) -> r.payload)

  let row =
    Row.(
      unit
        (fun
            id
             venue
             account
             kind
             source_id
             occurred_at
             occurred_on
             retrieved_at
             payload
           ->
           { id
           ; venue
           ; account
           ; kind
           ; source_id
           ; occurred_at
           ; occurred_on
           ; retrieved_at
           ; payload
           })
      * id
      * venue
      * account
      * kind
      * source_id
      * occurred_at
      * occurred_on
      * retrieved_at
      * payload)
  ;;

  let table =
    Table.make
      ~checks:
        Sqlite_schema.Check.
          [ canonical_instant occurred_at
          ; different (present occurred_at) (present occurred_on)
          ]
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~unique_keys:
        [ Table.Unique_key.make
            [ Col.Def venue; Col.Def account; Col.Def kind; Col.Def source_id ]
        ]
      ~indices:
        [ Table.Index.make ~name:"source_events_time" [ Col.Def occurred_at; Col.Def id ]
        ; (* The rows of one place and time, which a saved decision must not confuse. *)
          Table.Index.make
            ~name:"source_events_occurrence"
            [ Col.Def venue
            ; Col.Def account
            ; Col.Def kind
            ; Col.Def occurred_on
            ; Col.Def occurred_at
            ]
        ]
      "source_events"
      row
  ;;

  let definition = Sqlite_schema.table ~immutable:"source events are immutable" table
end

(* A bank movement as its adapter decoded it from the source event, stored once at
   import so that queries can use amounts and dates. Derived from the immutable
   event, so immutable too. The principal (amount plus fee) is split into its
   canonical magnitude and its sign, so that opposite amounts match exactly. *)
module Movements = struct
  type t =
    { event_id : int64
    ; venue : string
    ; account : string
    ; bank_id : string
    ; account_id : string
    ; account_type : string
    ; currency : string
    ; own_iban_status : string
    ; own_iban : string option
    ; booked_on : string
    ; value_on : string option
    ; amount : string
    ; fee : string
    ; principal_magnitude : string
    ; principal_sign : string
    ; counterparty_iban : string option
    ; descriptions : string
    ; movement_type : string option
    ; main_description : int
    }

  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)
  let venue = Col.make "venue" Type.text (fun (r : t) -> r.venue)
  let account = Col.make "account" Type.text (fun (r : t) -> r.account)
  let bank_id = Col.make "bank_id" Type.text (fun (r : t) -> r.bank_id)
  let account_id = Col.make "account_id" Type.text (fun (r : t) -> r.account_id)
  let account_type = Col.make "account_type" Type.text (fun (r : t) -> r.account_type)
  let currency = Col.make "currency" Type.text (fun (r : t) -> r.currency)

  let own_iban_status =
    Col.make "own_iban_status" Type.text (fun (r : t) -> r.own_iban_status)
  ;;

  let own_iban = Col.make "own_iban" Type.(option text) (fun (r : t) -> r.own_iban)
  let booked_on = Col.make "booked_on" Type.text (fun (r : t) -> r.booked_on)
  let value_on = Col.make "value_on" Type.(option text) (fun (r : t) -> r.value_on)
  let amount = Col.make "amount" Type.text (fun (r : t) -> r.amount)
  let fee = Col.make "fee" Type.text (fun (r : t) -> r.fee)

  let principal_magnitude =
    Col.make "principal_magnitude" Type.text (fun (r : t) -> r.principal_magnitude)
  ;;

  let principal_sign =
    Col.make "principal_sign" Type.text (fun (r : t) -> r.principal_sign)
  ;;

  let counterparty_iban =
    Col.make "counterparty_iban" Type.(option text) (fun (r : t) -> r.counterparty_iban)
  ;;

  (* A JSON array of strings, in source order. *)
  let descriptions = Col.make "descriptions" Type.text (fun (r : t) -> r.descriptions)

  let movement_type =
    Col.make "movement_type" Type.(option text) (fun (r : t) -> r.movement_type)
  ;;

  (* Which of the descriptions the adapter declares as the main text. *)
  let main_description =
    Col.make "main_description" Type.int (fun (r : t) -> r.main_description)
  ;;

  let row =
    Row.(
      unit
        (fun
            event_id
             venue
             account
             bank_id
             account_id
             account_type
             currency
             own_iban_status
             own_iban
             booked_on
             value_on
             amount
             fee
             principal_magnitude
             principal_sign
             counterparty_iban
             descriptions
             movement_type
             main_description
           ->
           { event_id
           ; venue
           ; account
           ; bank_id
           ; account_id
           ; account_type
           ; currency
           ; own_iban_status
           ; own_iban
           ; booked_on
           ; value_on
           ; amount
           ; fee
           ; principal_magnitude
           ; principal_sign
           ; counterparty_iban
           ; descriptions
           ; movement_type
           ; main_description
           })
      * event_id
      * venue
      * account
      * bank_id
      * account_id
      * account_type
      * currency
      * own_iban_status
      * own_iban
      * booked_on
      * value_on
      * amount
      * fee
      * principal_magnitude
      * principal_sign
      * counterparty_iban
      * descriptions
      * movement_type
      * main_description)
  ;;

  let table =
    Table.make
      ~checks:
        Sqlite_schema.Check.
          [ one_of principal_sign [ "+"; "-" ]
          ; one_of own_iban_status [ "known"; "unknown"; "not_applicable" ]
          ; equivalent (text_equal own_iban_status "known") (present own_iban)
          ; nonnegative main_description
          ]
      ~primary_key:(Table.Primary_key.make [ Col.Def event_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ]
      ~indices:
        [ Table.Index.make
            ~name:"movements_principal"
            [ Col.Def currency; Col.Def principal_magnitude ]
        ; Table.Index.make ~name:"movements_day" [ Col.Def currency; Col.Def booked_on ]
        ]
      "movements"
      row
  ;;

  let definition =
    Sqlite_schema.table
      ~immutable:"movements are derived from immutable source events"
      ~mutable_columns:[ "movement_type" ]
      table
  ;;
end

(* Every column or tag a source carried, by name, kept beside the immutable event so
   rules can match fields the payload does not hold. Not part of the event's identity. *)
module Operations = struct
  type t =
    { id : int64
    ; occurred : string
    ; bank_event_id : int64 option
    ; source_event_id : int64 option
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let occurred = Col.make "occurred" Type.text (fun (r : t) -> r.occurred)

  let bank_event_id =
    Col.make "bank_event_id" Type.(option int64) (fun (r : t) -> r.bank_event_id)
  ;;

  let source_event_id =
    Col.make "source_event_id" Type.(option int64) (fun (r : t) -> r.source_event_id)
  ;;

  let row =
    Row.(
      unit (fun id occurred bank_event_id source_event_id ->
        { id; occurred; bank_event_id; source_event_id })
      * id
      * occurred
      * bank_event_id
      * source_event_id)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~unique_keys:
        [ Table.Unique_key.make [ Col.Def bank_event_id ]
        ; Table.Unique_key.make [ Col.Def source_event_id ]
        ]
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def bank_event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def source_event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ]
      "operations"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Operation_sources = struct
  type t =
    { operation_id : int64
    ; event_id : int64
    }

  let operation_id = Col.make "operation_id" Type.int64 (fun (r : t) -> r.operation_id)
  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)

  let row =
    Row.(
      unit (fun operation_id event_id -> { operation_id; event_id })
      * operation_id
      * event_id)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def operation_id; Col.Def event_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def operation_id ]
            ~parent:
              (Table.Foreign_key.Table (Operations.table, [ Col.Def Operations.id ]))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ]
      ~indices:
        [ (* Which operations an event is a source of. *)
          Table.Index.make ~name:"operation_sources_event" [ Col.Def event_id ]
        ]
      "operation_sources"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Operation_movements = struct
  type t =
    { id : int64
    ; operation_id : int64
    ; anchor_event_id : int64
    ; role : string
    ; account : string option
    ; commodity : string
    ; quantity : string
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let operation_id = Col.make "operation_id" Type.int64 (fun (r : t) -> r.operation_id)

  let anchor_event_id =
    Col.make "anchor_event_id" Type.int64 (fun (r : t) -> r.anchor_event_id)
  ;;

  let role = Col.make "role" Type.text (fun (r : t) -> r.role)
  let account = Col.make "account" Type.(option text) (fun (r : t) -> r.account)
  let commodity = Col.make "commodity" Type.text (fun (r : t) -> r.commodity)
  let quantity = Col.make "quantity" Type.text (fun (r : t) -> r.quantity)

  let row =
    Row.(
      unit (fun id operation_id anchor_event_id role account commodity quantity ->
        { id; operation_id; anchor_event_id; role; account; commodity; quantity })
      * id
      * operation_id
      * anchor_event_id
      * role
      * account
      * commodity
      * quantity)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~unique_keys:
        [ Table.Unique_key.make [ Col.Def anchor_event_id; Col.Def role ]
        ; Table.Unique_key.make [ Col.Def id; Col.Def operation_id ]
        ; Table.Unique_key.make [ Col.Def id; Col.Def anchor_event_id ]
        ]
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def operation_id; Col.Def anchor_event_id ]
            ~parent:
              (Table.Foreign_key.Table
                 ( Operation_sources.table
                 , [ Col.Def Operation_sources.operation_id
                   ; Col.Def Operation_sources.event_id
                   ] ))
            ()
        ]
      ~indices:
        [ (* Most reads go from an operation to its movements. *)
          Table.Index.make ~name:"operation_movements_operation" [ Col.Def operation_id ]
        ]
      "operation_movements"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Operation_movement_sources = struct
  type t =
    { movement_id : int64
    ; operation_id : int64
    ; event_id : int64
    }

  let movement_id = Col.make "movement_id" Type.int64 (fun (r : t) -> r.movement_id)
  let operation_id = Col.make "operation_id" Type.int64 (fun (r : t) -> r.operation_id)
  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)

  let row =
    Row.(
      unit (fun movement_id operation_id event_id ->
        { movement_id; operation_id; event_id })
      * movement_id
      * operation_id
      * event_id)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def movement_id; Col.Def event_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def movement_id; Col.Def operation_id ]
            ~parent:
              (Table.Foreign_key.Table
                 ( Operation_movements.table
                 , [ Col.Def Operation_movements.id
                   ; Col.Def Operation_movements.operation_id
                   ] ))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def operation_id; Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table
                 ( Operation_sources.table
                 , [ Col.Def Operation_sources.operation_id
                   ; Col.Def Operation_sources.event_id
                   ] ))
            ()
        ]
      "operation_movement_sources"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Operation_transfer_links = struct
  type t =
    { first_movement_id : int64
    ; second_movement_id : int64
    ; reason : string
    }

  let first_movement_id =
    Col.make "first_movement_id" Type.int64 (fun (r : t) -> r.first_movement_id)
  ;;

  let second_movement_id =
    Col.make "second_movement_id" Type.int64 (fun (r : t) -> r.second_movement_id)
  ;;

  let reason = Col.make "reason" Type.text (fun (r : t) -> r.reason)

  let row =
    Row.(
      unit (fun first_movement_id second_movement_id reason ->
        { first_movement_id; second_movement_id; reason })
      * first_movement_id
      * second_movement_id
      * reason)
  ;;

  let table =
    Table.make
      ~checks:
        Sqlite_schema.Check.
          [ different
              (same first_movement_id second_movement_id)
              (same first_movement_id first_movement_id)
          ]
      ~primary_key:
        (Table.Primary_key.make [ Col.Def first_movement_id; Col.Def second_movement_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def first_movement_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Operation_movements.table, [ Col.Def Operation_movements.id ]))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def second_movement_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Operation_movements.table, [ Col.Def Operation_movements.id ]))
            ()
        ]
      "operation_transfer_links"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Operation_movement_resolutions = struct
  type t =
    { movement_id : int64
    ; account : string
    ; commodity : string
    }

  let movement_id = Col.make "movement_id" Type.int64 (fun (r : t) -> r.movement_id)
  let account = Col.make "account" Type.text (fun (r : t) -> r.account)
  let commodity = Col.make "commodity" Type.text (fun (r : t) -> r.commodity)

  let row =
    Row.(
      unit (fun movement_id account commodity -> { movement_id; account; commodity })
      * movement_id
      * account
      * commodity)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def movement_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def movement_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Operation_movements.table, [ Col.Def Operation_movements.id ]))
            ()
        ]
      "operation_movement_resolutions"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Commodity_symbols = struct
  type t =
    { commodity : string
    ; symbol : string
    }

  let commodity = Col.make "commodity" Type.text (fun (r : t) -> r.commodity)
  let symbol = Col.make "symbol" Type.text (fun (r : t) -> r.symbol)

  let row =
    Row.(unit (fun commodity symbol -> { commodity; symbol }) * commodity * symbol)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def commodity ])
      ~unique_keys:[ Table.Unique_key.make [ Col.Def symbol ] ]
      "commodity_symbols"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Operation_resolution_changes = struct
  type t =
    { id : int64
    ; movement_id : int64
    ; account : string
    ; commodity : string
    ; changed_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let movement_id = Col.make "movement_id" Type.int64 (fun (r : t) -> r.movement_id)
  let account = Col.make "account" Type.text (fun (r : t) -> r.account)
  let commodity = Col.make "commodity" Type.text (fun (r : t) -> r.commodity)

  let changed_at =
    Col.make ~default:timestamp "changed_at" instant (fun (r : t) -> r.changed_at)
  ;;

  let row =
    Row.(
      unit (fun id movement_id account commodity changed_at ->
        { id; movement_id; account; commodity; changed_at })
      * id
      * movement_id
      * account
      * commodity
      * changed_at)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def movement_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Operation_movements.table, [ Col.Def Operation_movements.id ]))
            ()
        ]
      "operation_resolution_changes"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"movement resolution history is append-only" table
  ;;
end

module Operation_transfer_memberships = struct
  type t =
    { movement_id : int64
    ; first_movement_id : int64
    ; second_movement_id : int64
    }

  let movement_id = Col.make "movement_id" Type.int64 (fun (r : t) -> r.movement_id)

  let first_movement_id =
    Col.make "first_movement_id" Type.int64 (fun (r : t) -> r.first_movement_id)
  ;;

  let second_movement_id =
    Col.make "second_movement_id" Type.int64 (fun (r : t) -> r.second_movement_id)
  ;;

  let row =
    Row.(
      unit (fun movement_id first_movement_id second_movement_id ->
        { movement_id; first_movement_id; second_movement_id })
      * movement_id
      * first_movement_id
      * second_movement_id)
  ;;

  let table =
    Table.make
      ~checks:
        Sqlite_schema.Check.
          [ either
              (same movement_id first_movement_id)
              (same movement_id second_movement_id)
          ]
      ~primary_key:(Table.Primary_key.make [ Col.Def movement_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def movement_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Operation_movements.table, [ Col.Def Operation_movements.id ]))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def first_movement_id; Col.Def second_movement_id ]
            ~parent:
              (Table.Foreign_key.Table
                 ( Operation_transfer_links.table
                 , [ Col.Def Operation_transfer_links.first_movement_id
                   ; Col.Def Operation_transfer_links.second_movement_id
                   ] ))
            ()
        ]
      "operation_transfer_memberships"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Operation_transfer_changes = struct
  type t =
    { id : int64
    ; first_movement_id : int64
    ; second_movement_id : int64
    ; action : string
    ; changed_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)

  let first_movement_id =
    Col.make "first_movement_id" Type.int64 (fun (r : t) -> r.first_movement_id)
  ;;

  let second_movement_id =
    Col.make "second_movement_id" Type.int64 (fun (r : t) -> r.second_movement_id)
  ;;

  let action = Col.make "action" Type.text (fun (r : t) -> r.action)

  let changed_at =
    Col.make ~default:timestamp "changed_at" instant (fun (r : t) -> r.changed_at)
  ;;

  let row =
    Row.(
      unit (fun id first_movement_id second_movement_id action changed_at ->
        { id; first_movement_id; second_movement_id; action; changed_at })
      * id
      * first_movement_id
      * second_movement_id
      * action
      * changed_at)
  ;;

  let table =
    Table.make
      ~checks:Sqlite_schema.Check.[ one_of action [ "confirmed"; "undone" ] ]
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def first_movement_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Operation_movements.table, [ Col.Def Operation_movements.id ]))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def second_movement_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Operation_movements.table, [ Col.Def Operation_movements.id ]))
            ()
        ]
      "operation_transfer_changes"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"operation transfer history is append-only" table
  ;;
end

module Operation_exchange_decisions = struct
  type t =
    { operation_id : int64
    ; reason : string
    }

  let operation_id = Col.make "operation_id" Type.int64 (fun (r : t) -> r.operation_id)
  let reason = Col.make "reason" Type.text (fun (r : t) -> r.reason)

  let row =
    Row.(
      unit (fun operation_id reason -> { operation_id; reason }) * operation_id * reason)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def operation_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def operation_id ]
            ~parent:
              (Table.Foreign_key.Table (Operations.table, [ Col.Def Operations.id ]))
            ()
        ]
      "operation_exchange_decisions"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Operation_exchange_changes = struct
  type t =
    { id : int64
    ; operation_id : int64
    ; action : string
    ; changed_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let operation_id = Col.make "operation_id" Type.int64 (fun (r : t) -> r.operation_id)
  let action = Col.make "action" Type.text (fun (r : t) -> r.action)

  let changed_at =
    Col.make ~default:timestamp "changed_at" instant (fun (r : t) -> r.changed_at)
  ;;

  let row =
    Row.(
      unit (fun id operation_id action changed_at ->
        { id; operation_id; action; changed_at })
      * id
      * operation_id
      * action
      * changed_at)
  ;;

  let table =
    Table.make
      ~checks:Sqlite_schema.Check.[ one_of action [ "confirmed"; "undone" ] ]
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def operation_id ]
            ~parent:
              (Table.Foreign_key.Table (Operations.table, [ Col.Def Operations.id ]))
            ()
        ]
      "operation_exchange_changes"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"operation exchange history is append-only" table
  ;;
end

module Source_event_fields = struct
  type t =
    { event_id : int64
    ; ordinal : int
    ; name : string
    ; value : string
    }

  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)
  let ordinal = Col.make "ordinal" Type.int (fun (r : t) -> r.ordinal)
  let name = Col.make "name" Type.text (fun (r : t) -> r.name)
  let value = Col.make "value" Type.text (fun (r : t) -> r.value)

  let row =
    Row.(
      unit (fun event_id ordinal name value -> { event_id; ordinal; name; value })
      * event_id
      * ordinal
      * name
      * value)
  ;;

  let table =
    Table.make
      ~checks:Sqlite_schema.Check.[ nonnegative ordinal ]
      ~primary_key:(Table.Primary_key.make [ Col.Def event_id; Col.Def ordinal ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ]
      "source_event_fields"
      row
  ;;

  let definition = Sqlite_schema.table table
end

(* What restore and export compare an event by, computed once at import: its payload's
   digest (a records row's identity, else the payload), its fields' digest, and the evidence
   that tells it from others of its day. See Event_digests. *)
module Source_event_digests = struct
  type t =
    { event_id : int64
    ; payload_sha256 : string
    ; fields_sha256 : string
    ; evidence : string
    }

  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)

  let payload_sha256 =
    Col.make "payload_sha256" Type.text (fun (r : t) -> r.payload_sha256)
  ;;

  let fields_sha256 = Col.make "fields_sha256" Type.text (fun (r : t) -> r.fields_sha256)
  let evidence = Col.make "evidence" Type.text (fun (r : t) -> r.evidence)

  let row =
    Row.(
      unit (fun event_id payload_sha256 fields_sha256 evidence ->
        { event_id; payload_sha256; fields_sha256; evidence })
      * event_id
      * payload_sha256
      * fields_sha256
      * evidence)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def event_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ]
      "source_event_digests"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Journal_entries = struct
  type t =
    { id : int64
    ; event_id : int64
    ; projection_key : string
    ; date : string
    ; description : string
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)

  let projection_key =
    Col.make "projection_key" Type.text (fun (r : t) -> r.projection_key)
  ;;

  let date = Col.make "date" Type.text (fun (r : t) -> r.date)
  let description = Col.make "description" Type.text (fun (r : t) -> r.description)

  let row =
    Row.(
      unit (fun id event_id projection_key date description ->
        { id; event_id; projection_key; date; description })
      * id
      * event_id
      * projection_key
      * date
      * description)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~unique_keys:[ Table.Unique_key.make [ Col.Def event_id; Col.Def projection_key ] ]
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ]
      ~indices:
        [ Table.Index.make
            ~name:"journal_entries_date"
            [ Col.Def date; Col.Def event_id; Col.Def projection_key ]
        ]
      "journal_entries"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Postings = struct
  type t =
    { entry_id : int64
    ; ordinal : int
    ; account : string
    ; commodity : string
    ; quantity : string
    }

  let entry_id = Col.make "entry_id" Type.int64 (fun (r : t) -> r.entry_id)
  let ordinal = Col.make "ordinal" Type.int (fun (r : t) -> r.ordinal)
  let account = Col.make "account" Type.text (fun (r : t) -> r.account)
  let commodity = Col.make "commodity" Type.text (fun (r : t) -> r.commodity)
  let quantity = Col.make "quantity" Type.text (fun (r : t) -> r.quantity)

  let row =
    Row.(
      unit (fun entry_id ordinal account commodity quantity ->
        { entry_id; ordinal; account; commodity; quantity })
      * entry_id
      * ordinal
      * account
      * commodity
      * quantity)
  ;;

  let table =
    Table.make
      ~checks:Sqlite_schema.Check.[ nonnegative ordinal ]
      ~primary_key:(Table.Primary_key.make [ Col.Def entry_id; Col.Def ordinal ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def entry_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Journal_entries.table, [ Col.Def Journal_entries.id ]))
            ()
        ]
      "postings"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Movement_evidence = struct
  type t =
    { event_id : int64
    ; card_reference : string option
    ; withdrawal_reversal : string
    }

  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)

  let card_reference =
    Col.make "card_reference" Type.(option text) (fun (r : t) -> r.card_reference)
  ;;

  let withdrawal_reversal =
    Col.make "withdrawal_reversal" Type.text (fun (r : t) -> r.withdrawal_reversal)
  ;;

  let row =
    Row.(
      unit (fun event_id card_reference withdrawal_reversal ->
        { event_id; card_reference; withdrawal_reversal })
      * event_id
      * card_reference
      * withdrawal_reversal)
  ;;

  let table =
    Table.make
      ~checks:Sqlite_schema.Check.[ one_of withdrawal_reversal [ "yes"; "no" ] ]
      ~primary_key:(Table.Primary_key.make [ Col.Def event_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ]
      "movement_evidence"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Import_documents = struct
  type t =
    { id : int64
    ; institution : string
    ; sha256 : string
    ; contents : string
    ; imported_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)
  let sha256 = Col.make "sha256" Type.text (fun (r : t) -> r.sha256)
  let contents = Col.make "contents" Type.blob (fun (r : t) -> r.contents)

  let imported_at =
    Col.make ~default:timestamp "imported_at" instant (fun (r : t) -> r.imported_at)
  ;;

  let row =
    Row.(
      unit (fun id institution sha256 contents imported_at ->
        { id; institution; sha256; contents; imported_at })
      * id
      * institution
      * sha256
      * contents
      * imported_at)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~unique_keys:[ Table.Unique_key.make [ Col.Def institution; Col.Def sha256 ] ]
      "import_documents"
      row
  ;;

  let definition = Sqlite_schema.table ~immutable:"import documents are immutable" table
end

module Import_document_events = struct
  type t =
    { document_id : int64
    ; event_id : int64
    }

  let document_id = Col.make "document_id" Type.int64 (fun (r : t) -> r.document_id)
  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)

  let row =
    Row.(
      unit (fun document_id event_id -> { document_id; event_id })
      * document_id
      * event_id)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def document_id; Col.Def event_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def document_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Import_documents.table, [ Col.Def Import_documents.id ]))
            ()
        ]
      ~indices:
        [ (* Which documents carried an event. *)
          Table.Index.make ~name:"import_document_events_event" [ Col.Def event_id ]
        ]
      "import_document_events"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Bank_account_mappings = struct
  type t =
    { institution : string
    ; source_account : string
    ; ledger_account : string
    ; public_id : string
    }

  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)

  let source_account =
    Col.make "source_account" Type.text (fun (r : t) -> r.source_account)
  ;;

  let ledger_account =
    Col.make "ledger_account" Type.text (fun (r : t) -> r.ledger_account)
  ;;

  (* [Bank_account_mapping.id]: the short ID commands show, stored because SQLite cannot
     compute it. *)
  let public_id = Col.make "public_id" Type.text (fun (r : t) -> r.public_id)

  let row =
    Row.(
      unit (fun institution source_account ledger_account public_id ->
        { institution; source_account; ledger_account; public_id })
      * institution
      * source_account
      * ledger_account
      * public_id)
  ;;

  let table =
    Table.make
      ~primary_key:
        (Table.Primary_key.make [ Col.Def institution; Col.Def source_account ])
      "bank_account_mappings"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Bank_account_mapping_changes = struct
  type t =
    { id : int64
    ; institution : string
    ; source_account : string
    ; previous_ledger_account : string option
    ; ledger_account : string
    ; changed_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)

  let source_account =
    Col.make "source_account" Type.text (fun (r : t) -> r.source_account)
  ;;

  let previous_ledger_account =
    Col.make
      "previous_ledger_account"
      Type.(option text)
      (fun (r : t) -> r.previous_ledger_account)
  ;;

  let ledger_account =
    Col.make "ledger_account" Type.text (fun (r : t) -> r.ledger_account)
  ;;

  let changed_at =
    Col.make ~default:timestamp "changed_at" instant (fun (r : t) -> r.changed_at)
  ;;

  let row =
    Row.(
      unit
        (fun
            id
             institution
             source_account
             previous_ledger_account
             ledger_account
             changed_at
           ->
           { id
           ; institution
           ; source_account
           ; previous_ledger_account
           ; ledger_account
           ; changed_at
           })
      * id
      * institution
      * source_account
      * previous_ledger_account
      * ledger_account
      * changed_at)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      "bank_account_mapping_changes"
      row
  ;;

  let definition = Sqlite_schema.table ~immutable:"mapping history is append-only" table
end

module Statements = struct
  type t =
    { id : int64
    ; document_id : int64
    ; ordinal : int
    ; institution : string
    ; source_account : string
    ; reference : string option
    ; currency : string
    ; period_start : Temporal.t
    ; period_end : Temporal.t
    ; opening_as_of : Temporal.t
    ; closing_as_of : Temporal.t
    ; opening : string
    ; closing : string
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let document_id = Col.make "document_id" Type.int64 (fun (r : t) -> r.document_id)
  let ordinal = Col.make "ordinal" Type.int (fun (r : t) -> r.ordinal)
  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)

  let source_account =
    Col.make "source_account" Type.text (fun (r : t) -> r.source_account)
  ;;

  let reference = Col.make "reference" Type.(option text) (fun (r : t) -> r.reference)
  let currency = Col.make "currency" Type.text (fun (r : t) -> r.currency)

  let period_start =
    Col.make "period_start" statement_time (fun (r : t) -> r.period_start)
  ;;

  let period_end = Col.make "period_end" statement_time (fun (r : t) -> r.period_end)

  let opening_as_of =
    Col.make "opening_as_of" statement_time (fun (r : t) -> r.opening_as_of)
  ;;

  let closing_as_of =
    Col.make "closing_as_of" statement_time (fun (r : t) -> r.closing_as_of)
  ;;

  let opening = Col.make "opening" Type.text (fun (r : t) -> r.opening)
  let closing = Col.make "closing" Type.text (fun (r : t) -> r.closing)

  let row =
    Row.(
      unit
        (fun
            id
             document_id
             ordinal
             institution
             source_account
             reference
             currency
             period_start
             period_end
             opening_as_of
             closing_as_of
             opening
             closing
           ->
           { id
           ; document_id
           ; ordinal
           ; institution
           ; source_account
           ; reference
           ; currency
           ; period_start
           ; period_end
           ; opening_as_of
           ; closing_as_of
           ; opening
           ; closing
           })
      * id
      * document_id
      * ordinal
      * institution
      * source_account
      * reference
      * currency
      * period_start
      * period_end
      * opening_as_of
      * closing_as_of
      * opening
      * closing)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~unique_keys:[ Table.Unique_key.make [ Col.Def document_id; Col.Def ordinal ] ]
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def document_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Import_documents.table, [ Col.Def Import_documents.id ]))
            ()
        ]
      "statements"
      row
  ;;

  let definition = Sqlite_schema.table ~immutable:"statements are immutable" table
end

module Stated_balances = struct
  type t =
    { document_id : int64
    ; institution : string
    ; account : string
    ; balance_type : string
    ; commodity_json : string
    ; quantity : string
    ; as_of : Temporal.t
    }

  let document_id = Col.make "document_id" Type.int64 (fun (r : t) -> r.document_id)
  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)
  let account = Col.make "account" Type.text (fun (r : t) -> r.account)
  let balance_type = Col.make "balance_type" Type.text (fun (r : t) -> r.balance_type)

  let commodity_json =
    Col.make "commodity_json" Type.text (fun (r : t) -> r.commodity_json)
  ;;

  let quantity = Col.make "quantity" Type.text (fun (r : t) -> r.quantity)
  let as_of = Col.make "as_of" statement_time (fun (r : t) -> r.as_of)

  let row =
    Row.(
      unit
        (fun document_id institution account balance_type commodity_json quantity as_of ->
           { document_id
           ; institution
           ; account
           ; balance_type
           ; commodity_json
           ; quantity
           ; as_of
           })
      * document_id
      * institution
      * account
      * balance_type
      * commodity_json
      * quantity
      * as_of)
  ;;

  let table =
    Table.make
      ~checks:
        Sqlite_schema.Check.
          [ one_of balance_type [ "opening"; "closing"; "available"; "day_end" ] ]
      ~primary_key:
        (Table.Primary_key.make
           [ Col.Def document_id
           ; Col.Def account
           ; Col.Def balance_type
           ; Col.Def commodity_json
           ; Col.Def as_of
           ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def document_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Import_documents.table, [ Col.Def Import_documents.id ]))
            ()
        ]
      "stated_balances"
      row
  ;;

  let definition = Sqlite_schema.table ~immutable:"stated balances are immutable" table
end

module Statement_events = struct
  type t =
    { statement_id : int64
    ; event_id : int64
    }

  let statement_id = Col.make "statement_id" Type.int64 (fun (r : t) -> r.statement_id)
  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)

  let row =
    Row.(
      unit (fun statement_id event_id -> { statement_id; event_id })
      * statement_id
      * event_id)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def statement_id; Col.Def event_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def statement_id ]
            ~parent:
              (Table.Foreign_key.Table (Statements.table, [ Col.Def Statements.id ]))
            ()
        ]
      "statement_events"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Companion_rows = struct
  type t =
    { id : int64
    ; document_id : int64
    ; row_number : int
    ; institution : string
    ; format : string
    ; fields_json : string
    ; display_name : string option
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let document_id = Col.make "document_id" Type.int64 (fun (r : t) -> r.document_id)
  let row_number = Col.make "row_number" Type.int (fun (r : t) -> r.row_number)
  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)
  let format = Col.make "format" Type.text (fun (r : t) -> r.format)
  let fields_json = Col.make "fields_json" Type.text (fun (r : t) -> r.fields_json)

  (* The name the companion gives this row, from its annotation at import, so that a
     query can read a movement's effective description without the companion's code. *)
  let display_name =
    Col.make "display_name" Type.(option text) (fun (r : t) -> r.display_name)
  ;;

  let row =
    Row.(
      unit (fun id document_id row_number institution format fields_json display_name ->
        { id; document_id; row_number; institution; format; fields_json; display_name })
      * id
      * document_id
      * row_number
      * institution
      * format
      * fields_json
      * display_name)
  ;;

  let table =
    Table.make
      ~checks:Sqlite_schema.Check.[ positive row_number ]
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~unique_keys:[ Table.Unique_key.make [ Col.Def document_id; Col.Def row_number ] ]
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def document_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Import_documents.table, [ Col.Def Import_documents.id ]))
            ()
        ]
      ~indices:
        [ (* A companion layout's rows. *)
          Table.Index.make
            ~name:"companion_rows_format"
            [ Col.Def institution; Col.Def format ]
        ]
      "companion_rows"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"companion observations are immutable" table
  ;;
end

module Companion_matches = struct
  type t =
    { row_id : int64
    ; event_id : int64 option
    ; state : string
    ; reason : string
    ; candidates_json : string
    ; policy : string
    ; updated_at : Ptime.t
    }

  let row_id = Col.make "row_id" Type.int64 (fun (r : t) -> r.row_id)
  let event_id = Col.make "event_id" Type.(option int64) (fun (r : t) -> r.event_id)
  let state = Col.make "state" Type.text (fun (r : t) -> r.state)
  let reason = Col.make "reason" Type.text (fun (r : t) -> r.reason)

  let candidates_json =
    Col.make "candidates_json" Type.text (fun (r : t) -> r.candidates_json)
  ;;

  let policy = Col.make "policy" Type.text (fun (r : t) -> r.policy)

  let updated_at =
    Col.make ~default:timestamp "updated_at" instant (fun (r : t) -> r.updated_at)
  ;;

  let row =
    Row.(
      unit (fun row_id event_id state reason candidates_json policy updated_at ->
        { row_id; event_id; state; reason; candidates_json; policy; updated_at })
      * row_id
      * event_id
      * state
      * reason
      * candidates_json
      * policy
      * updated_at)
  ;;

  let table =
    Table.make
      ~checks:
        Sqlite_schema.Check.
          [ one_of state [ "matched"; "unmatched"; "ambiguous"; "conflict" ]
          ; equivalent (one_of state [ "matched"; "conflict" ]) (present event_id)
          ]
      ~primary_key:(Table.Primary_key.make [ Col.Def row_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def row_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Companion_rows.table, [ Col.Def Companion_rows.id ]))
            ()
        ]
      ~indices:[ Table.Index.make ~name:"companion_matches_event" [ Col.Def event_id ] ]
      "companion_matches"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Companion_match_changes = struct
  type t =
    { id : int64
    ; row_id : int64
    ; previous_event_id : int64 option
    ; event_id : int64 option
    ; previous_state : string option
    ; state : string
    ; reason : string
    ; candidates_json : string
    ; policy : string
    ; changed_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let row_id = Col.make "row_id" Type.int64 (fun (r : t) -> r.row_id)

  let previous_event_id =
    Col.make "previous_event_id" Type.(option int64) (fun (r : t) -> r.previous_event_id)
  ;;

  let event_id = Col.make "event_id" Type.(option int64) (fun (r : t) -> r.event_id)

  let previous_state =
    Col.make "previous_state" Type.(option text) (fun (r : t) -> r.previous_state)
  ;;

  let state = Col.make "state" Type.text (fun (r : t) -> r.state)
  let reason = Col.make "reason" Type.text (fun (r : t) -> r.reason)

  let candidates_json =
    Col.make "candidates_json" Type.text (fun (r : t) -> r.candidates_json)
  ;;

  let policy = Col.make "policy" Type.text (fun (r : t) -> r.policy)

  let changed_at =
    Col.make ~default:timestamp "changed_at" instant (fun (r : t) -> r.changed_at)
  ;;

  let row =
    Row.(
      unit
        (fun
            id
             row_id
             previous_event_id
             event_id
             previous_state
             state
             reason
             candidates_json
             policy
             changed_at
           ->
           { id
           ; row_id
           ; previous_event_id
           ; event_id
           ; previous_state
           ; state
           ; reason
           ; candidates_json
           ; policy
           ; changed_at
           })
      * id
      * row_id
      * previous_event_id
      * event_id
      * previous_state
      * state
      * reason
      * candidates_json
      * policy
      * changed_at)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def previous_event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def row_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Companion_rows.table, [ Col.Def Companion_rows.id ]))
            ()
        ]
      "companion_match_changes"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"companion matching history is append-only" table
  ;;
end

module Category_catalog = struct
  type t = { account : string }

  let account = Col.make "account" Type.text (fun (r : t) -> r.account)
  let row = Row.(unit (fun account -> { account }) * account)

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def account ])
      "category_catalog"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Bank_categories = struct
  type t =
    { movement_id : int64
    ; event_id : int64
    ; account : string
    ; reason : string
    }

  let movement_id = Col.make "movement_id" Type.int64 (fun (r : t) -> r.movement_id)
  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)
  let account = Col.make "account" Type.text (fun (r : t) -> r.account)
  let reason = Col.make "reason" Type.text (fun (r : t) -> r.reason)

  let row =
    Row.(
      unit (fun movement_id event_id account reason ->
        { movement_id; event_id; account; reason })
      * movement_id
      * event_id
      * account
      * reason)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def movement_id ])
      ~unique_keys:[ Table.Unique_key.make [ Col.Def event_id ] ]
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def movement_id; Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table
                 ( Operation_movements.table
                 , [ Col.Def Operation_movements.id
                   ; Col.Def Operation_movements.anchor_event_id
                   ] ))
            ()
        ]
      "bank_categories"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Bank_category_changes = struct
  type t =
    { id : int64
    ; movement_id : int64
    ; event_id : int64
    ; previous_account : string option
    ; account : string
    ; reason : string
    ; changed_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let movement_id = Col.make "movement_id" Type.int64 (fun (r : t) -> r.movement_id)
  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)

  let previous_account =
    Col.make "previous_account" Type.(option text) (fun (r : t) -> r.previous_account)
  ;;

  let account = Col.make "account" Type.text (fun (r : t) -> r.account)
  let reason = Col.make "reason" Type.text (fun (r : t) -> r.reason)

  let changed_at =
    Col.make ~default:timestamp "changed_at" instant (fun (r : t) -> r.changed_at)
  ;;

  let row =
    Row.(
      unit (fun id movement_id event_id previous_account account reason changed_at ->
        { id; movement_id; event_id; previous_account; account; reason; changed_at })
      * id
      * movement_id
      * event_id
      * previous_account
      * account
      * reason
      * changed_at)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def movement_id; Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table
                 ( Operation_movements.table
                 , [ Col.Def Operation_movements.id
                   ; Col.Def Operation_movements.anchor_event_id
                   ] ))
            ()
        ]
      "bank_category_changes"
      row
  ;;

  let definition = Sqlite_schema.table ~immutable:"category history is append-only" table
end

module Day_inventories = struct
  type t =
    { institution : string
    ; source_account : string
    ; booked_on : string
    ; signature : string
    ; document_id : int64
    }

  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)

  let source_account =
    Col.make "source_account" Type.text (fun (r : t) -> r.source_account)
  ;;

  let booked_on = Col.make "booked_on" Type.text (fun (r : t) -> r.booked_on)
  let signature = Col.make "signature" Type.text (fun (r : t) -> r.signature)
  let document_id = Col.make "document_id" Type.int64 (fun (r : t) -> r.document_id)

  let row =
    Row.(
      unit (fun institution source_account booked_on signature document_id ->
        { institution; source_account; booked_on; signature; document_id })
      * institution
      * source_account
      * booked_on
      * signature
      * document_id)
  ;;

  let table =
    Table.make
      ~primary_key:
        (Table.Primary_key.make
           [ Col.Def institution; Col.Def source_account; Col.Def booked_on ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def document_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Import_documents.table, [ Col.Def Import_documents.id ]))
            ()
        ]
      "day_inventories"
      row
  ;;

  let definition = Sqlite_schema.table ~immutable:"daily inventories are immutable" table
end

module Bank_import_attempts = struct
  type t =
    { id : int64
    ; institution : string
    ; path : string
    ; document_id : int64 option
    ; format : string
    ; outcome : string
    ; inserted_count : int
    ; existing_count : int
    ; error : string option
    ; attempted_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)
  let path = Col.make "path" Type.text (fun (r : t) -> r.path)

  let document_id =
    Col.make "document_id" Type.(option int64) (fun (r : t) -> r.document_id)
  ;;

  let format = Col.make "format" Type.text (fun (r : t) -> r.format)
  let outcome = Col.make "outcome" Type.text (fun (r : t) -> r.outcome)

  let inserted_count =
    Col.make "inserted_count" Type.int (fun (r : t) -> r.inserted_count)
  ;;

  let existing_count =
    Col.make "existing_count" Type.int (fun (r : t) -> r.existing_count)
  ;;

  let error = Col.make "error" Type.(option text) (fun (r : t) -> r.error)

  let attempted_at =
    Col.make ~default:timestamp "attempted_at" instant (fun (r : t) -> r.attempted_at)
  ;;

  let row =
    Row.(
      unit
        (fun
            id
             institution
             path
             document_id
             format
             outcome
             inserted_count
             existing_count
             error
             attempted_at
           ->
           { id
           ; institution
           ; path
           ; document_id
           ; format
           ; outcome
           ; inserted_count
           ; existing_count
           ; error
           ; attempted_at
           })
      * id
      * institution
      * path
      * document_id
      * format
      * outcome
      * inserted_count
      * existing_count
      * error
      * attempted_at)
  ;;

  let table =
    Table.make
      ~checks:
        Sqlite_schema.Check.
          [ one_of
              outcome
              [ "imported"; "unchanged"; "rejected"; "unsupported"; "unreadable" ]
          ; nonnegative inserted_count
          ; nonnegative existing_count
          ; equivalent (text_equal outcome "unreadable") (absent document_id)
          ]
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def document_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Import_documents.table, [ Col.Def Import_documents.id ]))
            ()
        ]
      ~indices:
        [ Table.Index.make
            ~name:"bank_import_attempts_document"
            [ Col.Def document_id; Col.Def id ]
        ]
      "bank_import_attempts"
      row
  ;;

  let definition = Sqlite_schema.table ~immutable:"import history is append-only" table
end

module Document_contexts = struct
  type t =
    { document_id : int64
    ; context_json : string
    }

  let document_id = Col.make "document_id" Type.int64 (fun (r : t) -> r.document_id)
  let context_json = Col.make "context_json" Type.text (fun (r : t) -> r.context_json)

  let row =
    Row.(
      unit (fun document_id context_json -> { document_id; context_json })
      * document_id
      * context_json)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def document_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def document_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Import_documents.table, [ Col.Def Import_documents.id ]))
            ()
        ]
      "document_contexts"
      row
  ;;

  let definition = Sqlite_schema.table ~immutable:"document contexts are immutable" table
end

module Bank_transfer_decisions = struct
  type t =
    { first_event_id : int64
    ; second_event_id : int64
    ; state : string
    ; reason : string
    }

  let first_event_id =
    Col.make "first_event_id" Type.int64 (fun (r : t) -> r.first_event_id)
  ;;

  let second_event_id =
    Col.make "second_event_id" Type.int64 (fun (r : t) -> r.second_event_id)
  ;;

  let state = Col.make "state" Type.text (fun (r : t) -> r.state)
  let reason = Col.make "reason" Type.text (fun (r : t) -> r.reason)

  let row =
    Row.(
      unit (fun first_event_id second_event_id state reason ->
        { first_event_id; second_event_id; state; reason })
      * first_event_id
      * second_event_id
      * state
      * reason)
  ;;

  let table =
    Table.make
      ~checks:
        Sqlite_schema.Check.
          [ one_of state [ "confirmed"; "rejected" ]
          ; less first_event_id second_event_id
          ]
      ~primary_key:
        (Table.Primary_key.make [ Col.Def first_event_id; Col.Def second_event_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def second_event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def first_event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ]
      "bank_transfer_decisions"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Bank_transfer_memberships = struct
  type t =
    { event_id : int64
    ; first_event_id : int64
    ; second_event_id : int64
    }

  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)

  let first_event_id =
    Col.make "first_event_id" Type.int64 (fun (r : t) -> r.first_event_id)
  ;;

  let second_event_id =
    Col.make "second_event_id" Type.int64 (fun (r : t) -> r.second_event_id)
  ;;

  let row =
    Row.(
      unit (fun event_id first_event_id second_event_id ->
        { event_id; first_event_id; second_event_id })
      * event_id
      * first_event_id
      * second_event_id)
  ;;

  let table =
    Table.make
      ~checks:
        Sqlite_schema.Check.
          [ either (same event_id first_event_id) (same event_id second_event_id) ]
      ~primary_key:(Table.Primary_key.make [ Col.Def event_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def first_event_id; Col.Def second_event_id ]
            ~parent:
              (Table.Foreign_key.Table
                 ( Bank_transfer_decisions.table
                 , [ Col.Def Bank_transfer_decisions.first_event_id
                   ; Col.Def Bank_transfer_decisions.second_event_id
                   ] ))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ]
      "bank_transfer_memberships"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Bank_transfer_changes = struct
  type t =
    { id : int64
    ; first_event_id : int64
    ; second_event_id : int64
    ; previous_state : string option
    ; state : string
    ; reason : string
    ; changed_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)

  let first_event_id =
    Col.make "first_event_id" Type.int64 (fun (r : t) -> r.first_event_id)
  ;;

  let second_event_id =
    Col.make "second_event_id" Type.int64 (fun (r : t) -> r.second_event_id)
  ;;

  let previous_state =
    Col.make "previous_state" Type.(option text) (fun (r : t) -> r.previous_state)
  ;;

  let state = Col.make "state" Type.text (fun (r : t) -> r.state)
  let reason = Col.make "reason" Type.text (fun (r : t) -> r.reason)

  let changed_at =
    Col.make ~default:timestamp "changed_at" instant (fun (r : t) -> r.changed_at)
  ;;

  let row =
    Row.(
      unit
        (fun id first_event_id second_event_id previous_state state reason changed_at ->
           { id
           ; first_event_id
           ; second_event_id
           ; previous_state
           ; state
           ; reason
           ; changed_at
           })
      * id
      * first_event_id
      * second_event_id
      * previous_state
      * state
      * reason
      * changed_at)
  ;;

  let table =
    Table.make
      ~checks:Sqlite_schema.Check.[ one_of state [ "confirmed"; "rejected" ] ]
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def second_event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def first_event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ]
      "bank_transfer_changes"
      row
  ;;

  let definition = Sqlite_schema.table ~immutable:"Transfer history is append-only" table
end

module Bank_account_identities = struct
  type t =
    { institution : string
    ; source_account : string
    ; iban : string
    }

  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)

  let source_account =
    Col.make "source_account" Type.text (fun (r : t) -> r.source_account)
  ;;

  let iban = Col.make "iban" Type.text (fun (r : t) -> r.iban)

  let row =
    Row.(
      unit (fun institution source_account iban -> { institution; source_account; iban })
      * institution
      * source_account
      * iban)
  ;;

  let table =
    Table.make
      ~primary_key:
        (Table.Primary_key.make [ Col.Def institution; Col.Def source_account ])
      "bank_account_identities"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Bank_account_identity_changes = struct
  type t =
    { id : int64
    ; institution : string
    ; source_account : string
    ; previous_iban : string option
    ; iban : string option
    ; changed_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)

  let source_account =
    Col.make "source_account" Type.text (fun (r : t) -> r.source_account)
  ;;

  let previous_iban =
    Col.make "previous_iban" Type.(option text) (fun (r : t) -> r.previous_iban)
  ;;

  let iban = Col.make "iban" Type.(option text) (fun (r : t) -> r.iban)

  let changed_at =
    Col.make ~default:timestamp "changed_at" instant (fun (r : t) -> r.changed_at)
  ;;

  let row =
    Row.(
      unit (fun id institution source_account previous_iban iban changed_at ->
        { id; institution; source_account; previous_iban; iban; changed_at })
      * id
      * institution
      * source_account
      * previous_iban
      * iban
      * changed_at)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      "bank_account_identity_changes"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"Account identity history is append-only" table
  ;;
end

module Bank_account_settlements = struct
  type t =
    { institution : string
    ; source_account : string
    ; settlement_account : string
    }

  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)

  let source_account =
    Col.make "source_account" Type.text (fun (r : t) -> r.source_account)
  ;;

  let settlement_account =
    Col.make "settlement_account" Type.text (fun (r : t) -> r.settlement_account)
  ;;

  let row =
    Row.(
      unit (fun institution source_account settlement_account ->
        { institution; source_account; settlement_account })
      * institution
      * source_account
      * settlement_account)
  ;;

  let table =
    Table.make
      ~primary_key:
        (Table.Primary_key.make [ Col.Def institution; Col.Def source_account ])
      "bank_account_settlements"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Bank_account_settlement_changes = struct
  type t =
    { id : int64
    ; institution : string
    ; source_account : string
    ; previous_settlement : string option
    ; settlement_account : string option
    ; changed_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)

  let source_account =
    Col.make "source_account" Type.text (fun (r : t) -> r.source_account)
  ;;

  let previous_settlement =
    Col.make
      "previous_settlement"
      Type.(option text)
      (fun (r : t) -> r.previous_settlement)
  ;;

  let settlement_account =
    Col.make "settlement_account" Type.(option text) (fun (r : t) -> r.settlement_account)
  ;;

  let changed_at =
    Col.make ~default:timestamp "changed_at" instant (fun (r : t) -> r.changed_at)
  ;;

  let row =
    Row.(
      unit
        (fun
            id
             institution
             source_account
             previous_settlement
             settlement_account
             changed_at
           ->
           { id
           ; institution
           ; source_account
           ; previous_settlement
           ; settlement_account
           ; changed_at
           })
      * id
      * institution
      * source_account
      * previous_settlement
      * settlement_account
      * changed_at)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      "bank_account_settlement_changes"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"Account settlement history is append-only" table
  ;;
end

(* A card number, as the institution prints it, that a person says belongs to a card
   account. An account's own number needs no row: see the card_accounts view. *)
module Card_assignments = struct
  type t =
    { institution : string
    ; card_reference : string
    ; card_source_account : string
    }

  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)

  let card_reference =
    Col.make "card_reference" Type.text (fun (r : t) -> r.card_reference)
  ;;

  let card_source_account =
    Col.make "card_source_account" Type.text (fun (r : t) -> r.card_source_account)
  ;;

  let row =
    Row.(
      unit (fun institution card_reference card_source_account ->
        { institution; card_reference; card_source_account })
      * institution
      * card_reference
      * card_source_account)
  ;;

  let table =
    Table.make
      ~primary_key:
        (Table.Primary_key.make [ Col.Def institution; Col.Def card_reference ])
      "card_assignments"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Card_assignment_changes = struct
  type t =
    { id : int64
    ; institution : string
    ; card_reference : string
    ; previous_card_source_account : string option
    ; card_source_account : string option
    ; changed_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)

  let card_reference =
    Col.make "card_reference" Type.text (fun (r : t) -> r.card_reference)
  ;;

  let previous_card_source_account =
    Col.make
      "previous_card_source_account"
      Type.(option text)
      (fun (r : t) -> r.previous_card_source_account)
  ;;

  let card_source_account =
    Col.make
      "card_source_account"
      Type.(option text)
      (fun (r : t) -> r.card_source_account)
  ;;

  let changed_at =
    Col.make ~default:timestamp "changed_at" instant (fun (r : t) -> r.changed_at)
  ;;

  let row =
    Row.(
      unit
        (fun
            id
             institution
             card_reference
             previous_card_source_account
             card_source_account
             changed_at
           ->
           { id
           ; institution
           ; card_reference
           ; previous_card_source_account
           ; card_source_account
           ; changed_at
           })
      * id
      * institution
      * card_reference
      * previous_card_source_account
      * card_source_account
      * changed_at)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      "card_assignment_changes"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"Card assignment history is append-only" table
  ;;
end

module Card_settlement_decisions = struct
  type t =
    { event_id : int64
    ; card_institution : string
    ; card_source_account : string
    ; ledger_account : string
    ; reason : string
    }

  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)

  let card_institution =
    Col.make "card_institution" Type.text (fun (r : t) -> r.card_institution)
  ;;

  let card_source_account =
    Col.make "card_source_account" Type.text (fun (r : t) -> r.card_source_account)
  ;;

  let ledger_account =
    Col.make "ledger_account" Type.text (fun (r : t) -> r.ledger_account)
  ;;

  let reason = Col.make "reason" Type.text (fun (r : t) -> r.reason)

  let row =
    Row.(
      unit (fun event_id card_institution card_source_account ledger_account reason ->
        { event_id; card_institution; card_source_account; ledger_account; reason })
      * event_id
      * card_institution
      * card_source_account
      * ledger_account
      * reason)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def event_id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ]
      "card_settlement_decisions"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Card_settlement_changes = struct
  type t =
    { id : int64
    ; event_id : int64
    ; card_institution : string
    ; card_source_account : string
    ; ledger_account : string
    ; action : string
    ; changed_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)

  let card_institution =
    Col.make "card_institution" Type.text (fun (r : t) -> r.card_institution)
  ;;

  let card_source_account =
    Col.make "card_source_account" Type.text (fun (r : t) -> r.card_source_account)
  ;;

  let ledger_account =
    Col.make "ledger_account" Type.text (fun (r : t) -> r.ledger_account)
  ;;

  let action = Col.make "action" Type.text (fun (r : t) -> r.action)

  let changed_at =
    Col.make ~default:timestamp "changed_at" instant (fun (r : t) -> r.changed_at)
  ;;

  let row =
    Row.(
      unit
        (fun
            id
             event_id
             card_institution
             card_source_account
             ledger_account
             action
             changed_at
           ->
           { id
           ; event_id
           ; card_institution
           ; card_source_account
           ; ledger_account
           ; action
           ; changed_at
           })
      * id
      * event_id
      * card_institution
      * card_source_account
      * ledger_account
      * action
      * changed_at)
  ;;

  let table =
    Table.make
      ~checks:Sqlite_schema.Check.[ one_of action [ "confirmed"; "undone" ] ]
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      "card_settlement_changes"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"card settlement history is append-only" table
  ;;
end

module Classification_rules = struct
  type t =
    { mode : string
    ; account_type : string option
    ; priority : int
    ; name : string
    ; pattern : string option
    ; institution : string option
    ; match_type : string option
    ; case_mode : string
    ; action_kind : string
    ; target : string
    ; action_type : string option
    ; field : string option
    }

  let mode = Col.make "mode" Type.text (fun (r : t) -> r.mode)

  let account_type =
    Col.make "account_type" Type.(option text) (fun (r : t) -> r.account_type)
  ;;

  let priority = Col.make "priority" Type.int (fun (r : t) -> r.priority)
  let name = Col.make "name" Type.text (fun (r : t) -> r.name)
  let pattern = Col.make "pattern" Type.(option text) (fun (r : t) -> r.pattern)

  let institution =
    Col.make "institution" Type.(option text) (fun (r : t) -> r.institution)
  ;;

  let case_mode = Col.make "case_mode" Type.text (fun (r : t) -> r.case_mode)
  let match_type = Col.make "match_type" Type.(option text) (fun (r : t) -> r.match_type)
  let action_kind = Col.make "action_kind" Type.text (fun (r : t) -> r.action_kind)
  let target = Col.make "target" Type.text (fun (r : t) -> r.target)

  let action_type =
    Col.make "action_type" Type.(option text) (fun (r : t) -> r.action_type)
  ;;

  (* The field a type rule matches; the canonical description if absent. *)
  let field = Col.make "field" Type.(option text) (fun (r : t) -> r.field)

  let row =
    Row.(
      unit
        (fun
            mode
             account_type
             priority
             name
             pattern
             institution
             match_type
             case_mode
             action_kind
             target
             action_type
             field
           ->
           { mode
           ; account_type
           ; priority
           ; name
           ; pattern
           ; institution
           ; match_type
           ; case_mode
           ; action_kind
           ; target
           ; action_type
           ; field
           })
      * mode
      * account_type
      * priority
      * name
      * pattern
      * institution
      * match_type
      * case_mode
      * action_kind
      * target
      * action_type
      * field)
  ;;

  let table =
    Table.make
      ~checks:
        Sqlite_schema.Check.
          [ one_of mode [ "automatic"; "review"; "disabled" ]
          ; one_of case_mode [ "sensitive"; "insensitive" ]
          ; one_of
              action_kind
              [ "category"
              ; "settlement"
              ; "type"
              ; "type-category"
              ; "card-reference"
              ; "withdrawal-reversal"
              ]
          ]
      ~primary_key:(Table.Primary_key.make [ Col.Def name ])
      "classification_rules"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Institution_definitions = struct
  type t =
    { name : string
    ; definition : string
    }

  let name = Col.make "name" Type.text (fun (r : t) -> r.name)
  let definition_json = Col.make "definition_json" Type.text (fun (r : t) -> r.definition)

  let row =
    Row.(unit (fun name definition -> { name; definition }) * name * definition_json)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def name ])
      "institution_definitions"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Institutions = struct
  type t =
    { name : string
    ; profile : string option
    ; companion_profile : string option
    }

  let name = Col.make "name" Type.text (fun (r : t) -> r.name)
  let profile = Col.make "profile" Type.(option text) (fun (r : t) -> r.profile)

  let companion_profile =
    Col.make "companion_profile" Type.(option text) (fun (r : t) -> r.companion_profile)
  ;;

  let row =
    Row.(
      unit (fun name profile companion_profile -> { name; profile; companion_profile })
      * name
      * profile
      * companion_profile)
  ;;

  let table =
    Table.make ~primary_key:(Table.Primary_key.make [ Col.Def name ]) "institutions" row
  ;;

  let definition = Sqlite_schema.table table
end

module Reversal_pairs = struct
  type t =
    { original : int64
    ; reversal : int64
    ; reason : string
    }

  let original = Col.make "original" Type.int64 (fun (r : t) -> r.original)
  let reversal = Col.make "reversal" Type.int64 (fun (r : t) -> r.reversal)
  let reason = Col.make "reason" Type.text (fun (r : t) -> r.reason)

  let row =
    Row.(
      unit (fun original reversal reason -> { original; reversal; reason })
      * original
      * reversal
      * reason)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def original ])
      ~indices:
        [ Table.Index.make
            ~unique:true
            ~name:"reversal_pairs_reversal"
            [ Col.Def reversal ]
        ]
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def original ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ; Table.Foreign_key.make
            ~cols:[ Col.Def reversal ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
            ()
        ]
      "reversal_pairs"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Reversal_changes = struct
  type t =
    { id : int64
    ; original : int64
    ; reversal : int64
    ; action : string
    ; changed_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let original = Col.make "original" Type.int64 (fun (r : t) -> r.original)
  let reversal = Col.make "reversal" Type.int64 (fun (r : t) -> r.reversal)
  let action = Col.make "action" Type.text (fun (r : t) -> r.action)

  let changed_at =
    Col.make ~default:timestamp "changed_at" instant (fun (r : t) -> r.changed_at)
  ;;

  let row =
    Row.(
      unit (fun id original reversal action changed_at ->
        { id; original; reversal; action; changed_at })
      * id
      * original
      * reversal
      * action
      * changed_at)
  ;;

  let table =
    Table.make
      ~checks:Sqlite_schema.Check.[ one_of action [ "confirmed"; "undone" ] ]
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      "reversal_changes"
      row
  ;;

  let definition = Sqlite_schema.table ~immutable:"reversal history is append-only" table
end

module Movement_type_overrides = struct
  type t =
    { movement_id : int64
    ; event_id : int64
    ; movement_type : string
    }

  let movement_id = Col.make "movement_id" Type.int64 (fun (r : t) -> r.movement_id)
  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)
  let movement_type = Col.make "movement_type" Type.text (fun (r : t) -> r.movement_type)

  let row =
    Row.(
      unit (fun movement_id event_id movement_type ->
        { movement_id; event_id; movement_type })
      * movement_id
      * event_id
      * movement_type)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def movement_id ])
      ~unique_keys:[ Table.Unique_key.make [ Col.Def event_id ] ]
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def movement_id; Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table
                 ( Operation_movements.table
                 , [ Col.Def Operation_movements.id
                   ; Col.Def Operation_movements.anchor_event_id
                   ] ))
            ()
        ]
      "movement_type_overrides"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Movement_type_changes = struct
  type t =
    { id : int64
    ; movement_id : int64
    ; event_id : int64
    ; previous_type : string option
    ; movement_type : string option
    ; reason : string
    ; changed_at : Ptime.t
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let movement_id = Col.make "movement_id" Type.int64 (fun (r : t) -> r.movement_id)
  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)

  let previous_type =
    Col.make "previous_type" Type.(option text) (fun (r : t) -> r.previous_type)
  ;;

  let movement_type =
    Col.make "movement_type" Type.(option text) (fun (r : t) -> r.movement_type)
  ;;

  let changed_at =
    Col.make ~default:timestamp "changed_at" instant (fun (r : t) -> r.changed_at)
  ;;

  let reason = Col.make "reason" Type.text (fun (r : t) -> r.reason)

  let row =
    Row.(
      unit (fun id movement_id event_id previous_type movement_type reason changed_at ->
        { id; movement_id; event_id; previous_type; movement_type; reason; changed_at })
      * id
      * movement_id
      * event_id
      * previous_type
      * movement_type
      * reason
      * changed_at)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def movement_id; Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table
                 ( Operation_movements.table
                 , [ Col.Def Operation_movements.id
                   ; Col.Def Operation_movements.anchor_event_id
                   ] ))
            ()
        ]
      "movement_type_changes"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"movement type history is append-only" table
  ;;
end

let definitions =
  [ Source_events.definition
  ; Movements.definition
  ; Operations.definition
  ; Operation_sources.definition
  ; Operation_movements.definition
  ; Operation_movement_sources.definition
  ; Operation_transfer_links.definition
  ; Operation_transfer_memberships.definition
  ; Operation_movement_resolutions.definition
  ; Operation_resolution_changes.definition
  ; Commodity_symbols.definition
  ; Operation_exchange_decisions.definition
  ; Operation_exchange_changes.definition
  ; Operation_transfer_changes.definition
  ; Source_event_fields.definition
  ; Source_event_digests.definition
  ; Movement_evidence.definition
  ; Journal_entries.definition
  ; Postings.definition
  ; Import_documents.definition
  ; Import_document_events.definition
  ; Bank_account_mappings.definition
  ; Bank_account_mapping_changes.definition
  ; Statements.definition
  ; Stated_balances.definition
  ; Statement_events.definition
  ; Companion_rows.definition
  ; Companion_matches.definition
  ; Companion_match_changes.definition
  ; Category_catalog.definition
  ; Bank_categories.definition
  ; Bank_category_changes.definition
  ; Day_inventories.definition
  ; Bank_import_attempts.definition
  ; Document_contexts.definition
  ; Bank_transfer_decisions.definition
  ; Bank_transfer_memberships.definition
  ; Bank_transfer_changes.definition
  ; Bank_account_identities.definition
  ; Bank_account_identity_changes.definition
  ; Bank_account_settlements.definition
  ; Bank_account_settlement_changes.definition
  ; Institutions.definition
  ; Institution_definitions.definition
  ; Card_settlement_decisions.definition
  ; Card_settlement_changes.definition
  ; Card_assignments.definition
  ; Card_assignment_changes.definition
  ; Classification_rules.definition
  ; Reversal_pairs.definition
  ; Reversal_changes.definition
  ; Movement_type_overrides.definition
  ; Movement_type_changes.definition
  ]
;;

let schema = Schema.make ~tables:(List.map definitions ~f:Sqlite_schema.rel_table) ()

let create =
  String.concat ~sep:"\n" (List.concat_map definitions ~f:Sqlite_schema.statements)
  ^ Printf.sprintf "\nPRAGMA user_version = %d;" version
;;
