open Base
open Rel

(** One current experimental baseline; older databases must be reimported. Banks
    have no tables of their own. *)
let version = 11

let timestamp = `Expr "strftime('%Y-%m-%dT%H:%M:%fZ','now')"

module Source_events = struct
  type t =
    { id : int64
    ; venue : string
    ; account : string
    ; kind : string
    ; source_id : string
    ; occurred_at_ms : int64 option
    ; occurred_on : string option
    ; retrieved_at : string
    ; payload : string
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let venue = Col.make "venue" Type.text (fun (r : t) -> r.venue)
  let account = Col.make "account" Type.text (fun (r : t) -> r.account)
  let kind = Col.make "kind" Type.text (fun (r : t) -> r.kind)
  let source_id = Col.make "source_id" Type.text (fun (r : t) -> r.source_id)

  let occurred_at_ms =
    Col.make "occurred_at_ms" Type.(option int64) (fun (r : t) -> r.occurred_at_ms)
  ;;

  let occurred_on =
    Col.make "occurred_on" Type.(option text) (fun (r : t) -> r.occurred_on)
  ;;

  let retrieved_at =
    Col.make ~default:timestamp "retrieved_at" Type.text (fun (r : t) -> r.retrieved_at)
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
             occurred_at_ms
             occurred_on
             retrieved_at
             payload
           ->
           { id
           ; venue
           ; account
           ; kind
           ; source_id
           ; occurred_at_ms
           ; occurred_on
           ; retrieved_at
           ; payload
           })
      * id
      * venue
      * account
      * kind
      * source_id
      * occurred_at_ms
      * occurred_on
      * retrieved_at
      * payload)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def id ])
      ~unique_keys:
        [ Table.Unique_key.make
            [ Col.Def venue; Col.Def account; Col.Def kind; Col.Def source_id ]
        ]
      ~indices:
        [ Table.Index.make
            ~name:"source_events_time"
            [ Col.Def occurred_at_ms; Col.Def id ]
        ]
      "source_events"
      row
  ;;

  let definition =
    Sqlite_schema.table
      ~checks:
        Sqlite_schema.Check.
          [ nonnegative_int64_option occurred_at_ms
          ; different (present occurred_at_ms) (present occurred_on)
          ]
      ~immutable:"source events are immutable"
      table
  ;;
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

  let definition =
    Sqlite_schema.table ~checks:Sqlite_schema.Check.[ nonnegative ordinal ] table
  ;;
end

module Import_documents = struct
  type t =
    { id : int64
    ; institution : string
    ; sha256 : string
    ; contents : string
    ; imported_at : string
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)
  let sha256 = Col.make "sha256" Type.text (fun (r : t) -> r.sha256)
  let contents = Col.make "contents" Type.blob (fun (r : t) -> r.contents)

  let imported_at =
    Col.make ~default:timestamp "imported_at" Type.text (fun (r : t) -> r.imported_at)
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
    }

  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)

  let source_account =
    Col.make "source_account" Type.text (fun (r : t) -> r.source_account)
  ;;

  let ledger_account =
    Col.make "ledger_account" Type.text (fun (r : t) -> r.ledger_account)
  ;;

  let row =
    Row.(
      unit (fun institution source_account ledger_account ->
        { institution; source_account; ledger_account })
      * institution
      * source_account
      * ledger_account)
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
    ; changed_at : string
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
    Col.make ~default:timestamp "changed_at" Type.text (fun (r : t) -> r.changed_at)
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
    ; period_start : string
    ; period_end : string
    ; opening_as_of : string
    ; closing_as_of : string
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
  let period_start = Col.make "period_start" Type.text (fun (r : t) -> r.period_start)
  let period_end = Col.make "period_end" Type.text (fun (r : t) -> r.period_end)
  let opening_as_of = Col.make "opening_as_of" Type.text (fun (r : t) -> r.opening_as_of)
  let closing_as_of = Col.make "closing_as_of" Type.text (fun (r : t) -> r.closing_as_of)
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
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let document_id = Col.make "document_id" Type.int64 (fun (r : t) -> r.document_id)
  let row_number = Col.make "row_number" Type.int (fun (r : t) -> r.row_number)
  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)
  let format = Col.make "format" Type.text (fun (r : t) -> r.format)
  let fields_json = Col.make "fields_json" Type.text (fun (r : t) -> r.fields_json)

  let row =
    Row.(
      unit (fun id document_id row_number institution format fields_json ->
        { id; document_id; row_number; institution; format; fields_json })
      * id
      * document_id
      * row_number
      * institution
      * format
      * fields_json)
  ;;

  let table =
    Table.make
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
      "companion_rows"
      row
  ;;

  let definition =
    Sqlite_schema.table
      ~checks:Sqlite_schema.Check.[ positive row_number ]
      ~immutable:"companion observations are immutable"
      table
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
    ; updated_at : string
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
    Col.make ~default:timestamp "updated_at" Type.text (fun (r : t) -> r.updated_at)
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
      "companion_matches"
      row
  ;;

  let definition =
    Sqlite_schema.table
      ~checks:
        Sqlite_schema.Check.
          [ one_of state [ "matched"; "unmatched"; "ambiguous"; "conflict" ]
          ; equivalent (one_of state [ "matched"; "conflict" ]) (present event_id)
          ]
      table
  ;;
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
    ; changed_at : string
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
    Col.make ~default:timestamp "changed_at" Type.text (fun (r : t) -> r.changed_at)
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

module Bank_categories = struct
  type t =
    { event_id : int64
    ; account : string
    ; reason : string
    }

  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)
  let account = Col.make "account" Type.text (fun (r : t) -> r.account)
  let reason = Col.make "reason" Type.text (fun (r : t) -> r.reason)

  let row =
    Row.(
      unit (fun event_id account reason -> { event_id; account; reason })
      * event_id
      * account
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
      "bank_categories"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Bank_category_changes = struct
  type t =
    { id : int64
    ; event_id : int64
    ; previous_account : string option
    ; account : string
    ; reason : string
    ; changed_at : string
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let event_id = Col.make "event_id" Type.int64 (fun (r : t) -> r.event_id)

  let previous_account =
    Col.make "previous_account" Type.(option text) (fun (r : t) -> r.previous_account)
  ;;

  let account = Col.make "account" Type.text (fun (r : t) -> r.account)
  let reason = Col.make "reason" Type.text (fun (r : t) -> r.reason)

  let changed_at =
    Col.make ~default:timestamp "changed_at" Type.text (fun (r : t) -> r.changed_at)
  ;;

  let row =
    Row.(
      unit (fun id event_id previous_account account reason changed_at ->
        { id; event_id; previous_account; account; reason; changed_at })
      * id
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
            ~cols:[ Col.Def event_id ]
            ~parent:
              (Table.Foreign_key.Table (Source_events.table, [ Col.Def Source_events.id ]))
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
    ; attempted_at : string
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
    Col.make ~default:timestamp "attempted_at" Type.text (fun (r : t) -> r.attempted_at)
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

  let definition =
    Sqlite_schema.table
      ~checks:
        Sqlite_schema.Check.
          [ one_of
              outcome
              [ "imported"; "unchanged"; "rejected"; "unsupported"; "unreadable" ]
          ; nonnegative inserted_count
          ; nonnegative existing_count
          ; equivalent (text_equal outcome "unreadable") (absent document_id)
          ]
      ~immutable:"import history is append-only"
      table
  ;;
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

  let definition =
    Sqlite_schema.table
      ~checks:
        Sqlite_schema.Check.
          [ one_of state [ "confirmed"; "rejected" ]
          ; less first_event_id second_event_id
          ]
      table
  ;;
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

  let definition =
    Sqlite_schema.table
      ~checks:
        Sqlite_schema.Check.
          [ either (same event_id first_event_id) (same event_id second_event_id) ]
      table
  ;;
end

module Bank_transfer_changes = struct
  type t =
    { id : int64
    ; first_event_id : int64
    ; second_event_id : int64
    ; previous_state : string option
    ; state : string
    ; reason : string
    ; changed_at : string
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
    Col.make ~default:timestamp "changed_at" Type.text (fun (r : t) -> r.changed_at)
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

  let definition =
    Sqlite_schema.table
      ~checks:Sqlite_schema.Check.[ one_of state [ "confirmed"; "rejected" ] ]
      ~immutable:"Transfer history is append-only"
      table
  ;;
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
    ; changed_at : string
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
    Col.make ~default:timestamp "changed_at" Type.text (fun (r : t) -> r.changed_at)
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
    ; changed_at : string
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
    Col.make ~default:timestamp "changed_at" Type.text (fun (r : t) -> r.changed_at)
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

let definitions =
  [ Source_events.definition
  ; Journal_entries.definition
  ; Postings.definition
  ; Import_documents.definition
  ; Import_document_events.definition
  ; Bank_account_mappings.definition
  ; Bank_account_mapping_changes.definition
  ; Statements.definition
  ; Statement_events.definition
  ; Companion_rows.definition
  ; Companion_matches.definition
  ; Companion_match_changes.definition
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
  ]
;;

let schema = Schema.make ~tables:(List.map definitions ~f:Sqlite_schema.rel_table) ()

let create =
  String.concat ~sep:"\n" (List.concat_map definitions ~f:Sqlite_schema.statements)
  ^ Printf.sprintf "\nPRAGMA user_version = %d;" version
;;
