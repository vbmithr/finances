open Core
open Rel

(** One current experimental baseline; older databases must be reimported. *)
let version = 10

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

module Bank_csv_rows = struct
  type t =
    { id : int64
    ; document_id : int64
    ; row_number : int
    ; institution : string
    ; account_id : string
    ; booked_on : string
    ; value_on : string option
    ; amount : string
    ; description : string
    ; suggested_description : string option
    ; category : string option
    ; parent_category : string option
    ; comment : string option
    ; account_label : string option
    ; reported_balance : string option
    ; pointage : string option
    ; fields_json : string
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let document_id = Col.make "document_id" Type.int64 (fun (r : t) -> r.document_id)
  let row_number = Col.make "row_number" Type.int (fun (r : t) -> r.row_number)
  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)
  let account_id = Col.make "account_id" Type.text (fun (r : t) -> r.account_id)
  let booked_on = Col.make "booked_on" Type.text (fun (r : t) -> r.booked_on)
  let value_on = Col.make "value_on" Type.(option text) (fun (r : t) -> r.value_on)
  let amount = Col.make "amount" Type.text (fun (r : t) -> r.amount)
  let description = Col.make "description" Type.text (fun (r : t) -> r.description)

  let suggested_description =
    Col.make
      "suggested_description"
      Type.(option text)
      (fun (r : t) -> r.suggested_description)
  ;;

  let category = Col.make "category" Type.(option text) (fun (r : t) -> r.category)

  let parent_category =
    Col.make "parent_category" Type.(option text) (fun (r : t) -> r.parent_category)
  ;;

  let comment = Col.make "comment" Type.(option text) (fun (r : t) -> r.comment)

  let account_label =
    Col.make "account_label" Type.(option text) (fun (r : t) -> r.account_label)
  ;;

  let reported_balance =
    Col.make "reported_balance" Type.(option text) (fun (r : t) -> r.reported_balance)
  ;;

  let pointage = Col.make "pointage" Type.(option text) (fun (r : t) -> r.pointage)
  let fields_json = Col.make "fields_json" Type.text (fun (r : t) -> r.fields_json)

  let row =
    Row.(
      unit
        (fun
            id
             document_id
             row_number
             institution
             account_id
             booked_on
             value_on
             amount
             description
             suggested_description
             category
             parent_category
             comment
             account_label
             reported_balance
             pointage
             fields_json
           ->
           { id
           ; document_id
           ; row_number
           ; institution
           ; account_id
           ; booked_on
           ; value_on
           ; amount
           ; description
           ; suggested_description
           ; category
           ; parent_category
           ; comment
           ; account_label
           ; reported_balance
           ; pointage
           ; fields_json
           })
      * id
      * document_id
      * row_number
      * institution
      * account_id
      * booked_on
      * value_on
      * amount
      * description
      * suggested_description
      * category
      * parent_category
      * comment
      * account_label
      * reported_balance
      * pointage
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
      ~indices:
        [ Table.Index.make
            ~name:"bank_csv_rows_account_date"
            [ Col.Def institution; Col.Def account_id; Col.Def booked_on ]
        ]
      "bank_csv_rows"
      row
  ;;

  let definition =
    Sqlite_schema.table
      ~checks:Sqlite_schema.Check.[ positive row_number ]
      ~immutable:"CSV observations are immutable"
      table
  ;;
end

module Bank_csv_matches = struct
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
              (Table.Foreign_key.Table (Bank_csv_rows.table, [ Col.Def Bank_csv_rows.id ]))
            ()
        ]
      "bank_csv_matches"
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

module Bank_csv_match_changes = struct
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
              (Table.Foreign_key.Table (Bank_csv_rows.table, [ Col.Def Bank_csv_rows.id ]))
            ()
        ]
      "bank_csv_match_changes"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"CSV matching history is append-only" table
  ;;
end

module Wise_statements = struct
  type t =
    { id : int64
    ; document_id : int64
    ; ordinal : int
    ; institution : string
    ; source_account : string
    ; statement_reference : string
    ; currency : string
    ; period_start : string
    ; period_end : string
    ; opening_at : string
    ; closing_at : string
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

  let statement_reference =
    Col.make "statement_reference" Type.text (fun (r : t) -> r.statement_reference)
  ;;

  let currency = Col.make "currency" Type.text (fun (r : t) -> r.currency)
  let period_start = Col.make "period_start" Type.text (fun (r : t) -> r.period_start)
  let period_end = Col.make "period_end" Type.text (fun (r : t) -> r.period_end)
  let opening_at = Col.make "opening_at" Type.text (fun (r : t) -> r.opening_at)
  let closing_at = Col.make "closing_at" Type.text (fun (r : t) -> r.closing_at)
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
             statement_reference
             currency
             period_start
             period_end
             opening_at
             closing_at
             opening
             closing
           ->
           { id
           ; document_id
           ; ordinal
           ; institution
           ; source_account
           ; statement_reference
           ; currency
           ; period_start
           ; period_end
           ; opening_at
           ; closing_at
           ; opening
           ; closing
           })
      * id
      * document_id
      * ordinal
      * institution
      * source_account
      * statement_reference
      * currency
      * period_start
      * period_end
      * opening_at
      * closing_at
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
      "wise_statements"
      row
  ;;

  let definition = Sqlite_schema.table ~immutable:"Wise statements are immutable" table
end

module Wise_statement_events = struct
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
              (Table.Foreign_key.Table
                 (Wise_statements.table, [ Col.Def Wise_statements.id ]))
            ()
        ]
      "wise_statement_events"
      row
  ;;

  let definition = Sqlite_schema.table table
end

module Wise_csv_rows = struct
  type t =
    { id : int64
    ; document_id : int64
    ; row_number : int
    ; institution : string
    ; fields_json : string
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let document_id = Col.make "document_id" Type.int64 (fun (r : t) -> r.document_id)
  let row_number = Col.make "row_number" Type.int (fun (r : t) -> r.row_number)
  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)
  let fields_json = Col.make "fields_json" Type.text (fun (r : t) -> r.fields_json)

  let row =
    Row.(
      unit (fun id document_id row_number institution fields_json ->
        { id; document_id; row_number; institution; fields_json })
      * id
      * document_id
      * row_number
      * institution
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
      "wise_csv_rows"
      row
  ;;

  let definition =
    Sqlite_schema.table
      ~checks:Sqlite_schema.Check.[ positive row_number ]
      ~immutable:"Wise CSV observations are immutable"
      table
  ;;
end

module Wise_csv_matches = struct
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
              (Table.Foreign_key.Table (Wise_csv_rows.table, [ Col.Def Wise_csv_rows.id ]))
            ()
        ]
      "wise_csv_matches"
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

module Wise_csv_match_changes = struct
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
              (Table.Foreign_key.Table (Wise_csv_rows.table, [ Col.Def Wise_csv_rows.id ]))
            ()
        ]
      "wise_csv_match_changes"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"Wise matching history is append-only" table
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

module Bunq_day_inventories = struct
  type t =
    { source_account : string
    ; booked_on : string
    ; signature : string
    ; document_id : int64
    }

  let source_account =
    Col.make "source_account" Type.text (fun (r : t) -> r.source_account)
  ;;

  let booked_on = Col.make "booked_on" Type.text (fun (r : t) -> r.booked_on)
  let signature = Col.make "signature" Type.text (fun (r : t) -> r.signature)
  let document_id = Col.make "document_id" Type.int64 (fun (r : t) -> r.document_id)

  let row =
    Row.(
      unit (fun source_account booked_on signature document_id ->
        { source_account; booked_on; signature; document_id })
      * source_account
      * booked_on
      * signature
      * document_id)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def source_account; Col.Def booked_on ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def document_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Import_documents.table, [ Col.Def Import_documents.id ]))
            ()
        ]
      "bunq_day_inventories"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"Bunq daily inventories are immutable" table
  ;;
end

module Bunq_statements = struct
  type t =
    { document_id : int64
    ; account_id : string
    ; currency : string
    ; opening_on : string
    ; closing_on : string
    ; opening : string
    ; closing : string
    }

  let document_id = Col.make "document_id" Type.int64 (fun (r : t) -> r.document_id)
  let account_id = Col.make "account_id" Type.text (fun (r : t) -> r.account_id)
  let currency = Col.make "currency" Type.text (fun (r : t) -> r.currency)
  let opening_on = Col.make "opening_on" Type.text (fun (r : t) -> r.opening_on)
  let closing_on = Col.make "closing_on" Type.text (fun (r : t) -> r.closing_on)
  let opening = Col.make "opening" Type.text (fun (r : t) -> r.opening)
  let closing = Col.make "closing" Type.text (fun (r : t) -> r.closing)

  let row =
    Row.(
      unit (fun document_id account_id currency opening_on closing_on opening closing ->
        { document_id; account_id; currency; opening_on; closing_on; opening; closing })
      * document_id
      * account_id
      * currency
      * opening_on
      * closing_on
      * opening
      * closing)
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
      "bunq_statements"
      row
  ;;

  let definition = Sqlite_schema.table ~immutable:"Bunq statements are immutable" table
end

module Bunq_csv_rows = struct
  type t =
    { id : int64
    ; document_id : int64
    ; row_number : int
    ; institution : string
    ; fields_json : string
    }

  let id = Col.make "id" Type.int64 (fun (r : t) -> r.id)
  let document_id = Col.make "document_id" Type.int64 (fun (r : t) -> r.document_id)
  let row_number = Col.make "row_number" Type.int (fun (r : t) -> r.row_number)
  let institution = Col.make "institution" Type.text (fun (r : t) -> r.institution)
  let fields_json = Col.make "fields_json" Type.text (fun (r : t) -> r.fields_json)

  let row =
    Row.(
      unit (fun id document_id row_number institution fields_json ->
        { id; document_id; row_number; institution; fields_json })
      * id
      * document_id
      * row_number
      * institution
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
      "bunq_csv_rows"
      row
  ;;

  let definition =
    Sqlite_schema.table
      ~checks:Sqlite_schema.Check.[ positive row_number ]
      ~immutable:"Bunq CSV observations are immutable"
      table
  ;;
end

module Bunq_csv_matches = struct
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
              (Table.Foreign_key.Table (Bunq_csv_rows.table, [ Col.Def Bunq_csv_rows.id ]))
            ()
        ]
      "bunq_csv_matches"
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

module Bunq_csv_match_changes = struct
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
              (Table.Foreign_key.Table (Bunq_csv_rows.table, [ Col.Def Bunq_csv_rows.id ]))
            ()
        ]
      "bunq_csv_match_changes"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"Bunq matching history is append-only" table
  ;;
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
              format
              [ "ofx"
              ; "csv"
              ; "qif"
              ; "spreadsheet"
              ; "unknown"
              ; "camt053"
              ; "wise_csv"
              ; "bunq_mt940"
              ; "bunq_csv"
              ; "revolut_csv"
              ]
          ; one_of
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

module Revolut_documents = struct
  type t =
    { document_id : int64
    ; account_id : string
    }

  let document_id = Col.make "document_id" Type.int64 (fun (r : t) -> r.document_id)
  let account_id = Col.make "account_id" Type.text (fun (r : t) -> r.account_id)

  let row =
    Row.(
      unit (fun document_id account_id -> { document_id; account_id })
      * document_id
      * account_id)
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
      "revolut_documents"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"Revolut document context is immutable" table
  ;;
end

module Revolut_day_inventories = struct
  type t =
    { source_account : string
    ; booked_on : string
    ; signature : string
    ; document_id : int64
    }

  let source_account =
    Col.make "source_account" Type.text (fun (r : t) -> r.source_account)
  ;;

  let booked_on = Col.make "booked_on" Type.text (fun (r : t) -> r.booked_on)
  let signature = Col.make "signature" Type.text (fun (r : t) -> r.signature)
  let document_id = Col.make "document_id" Type.int64 (fun (r : t) -> r.document_id)

  let row =
    Row.(
      unit (fun source_account booked_on signature document_id ->
        { source_account; booked_on; signature; document_id })
      * source_account
      * booked_on
      * signature
      * document_id)
  ;;

  let table =
    Table.make
      ~primary_key:(Table.Primary_key.make [ Col.Def source_account; Col.Def booked_on ])
      ~foreign_keys:
        [ Table.Foreign_key.make
            ~cols:[ Col.Def document_id ]
            ~parent:
              (Table.Foreign_key.Table
                 (Import_documents.table, [ Col.Def Import_documents.id ]))
            ()
        ]
      "revolut_day_inventories"
      row
  ;;

  let definition =
    Sqlite_schema.table ~immutable:"Revolut day inventories are immutable" table
  ;;
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
  ; Bank_csv_rows.definition
  ; Bank_csv_matches.definition
  ; Bank_csv_match_changes.definition
  ; Wise_statements.definition
  ; Wise_statement_events.definition
  ; Wise_csv_rows.definition
  ; Wise_csv_matches.definition
  ; Wise_csv_match_changes.definition
  ; Bank_categories.definition
  ; Bank_category_changes.definition
  ; Bunq_day_inventories.definition
  ; Bunq_statements.definition
  ; Bunq_csv_rows.definition
  ; Bunq_csv_matches.definition
  ; Bunq_csv_match_changes.definition
  ; Bank_import_attempts.definition
  ; Revolut_documents.definition
  ; Revolut_day_inventories.definition
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
