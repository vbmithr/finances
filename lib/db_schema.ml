let create =
  {|
CREATE TABLE source_events (
  id INTEGER PRIMARY KEY,
  venue TEXT NOT NULL,
  account TEXT NOT NULL,
  kind TEXT NOT NULL,
  source_id TEXT NOT NULL,
  occurred_at_ms INTEGER CHECK (occurred_at_ms >= 0),
  occurred_on TEXT,
  retrieved_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  payload BLOB NOT NULL,
  CHECK ((occurred_at_ms IS NOT NULL) != (occurred_on IS NOT NULL)),
  UNIQUE (venue, account, kind, source_id)
) STRICT;
CREATE INDEX source_events_time ON source_events (occurred_at_ms, id);
CREATE TABLE journal_entries (
  id INTEGER PRIMARY KEY,
  event_id INTEGER NOT NULL REFERENCES source_events(id),
  projection_key TEXT NOT NULL,
  date TEXT NOT NULL,
  description TEXT NOT NULL,
  UNIQUE (event_id, projection_key)
) STRICT;
CREATE TABLE postings (
  entry_id INTEGER NOT NULL REFERENCES journal_entries(id),
  ordinal INTEGER NOT NULL CHECK (ordinal >= 0),
  account TEXT NOT NULL,
  commodity TEXT NOT NULL,
  quantity TEXT NOT NULL,
  PRIMARY KEY (entry_id, ordinal)
) STRICT;
CREATE INDEX journal_entries_date ON journal_entries (date, event_id, projection_key);
CREATE TRIGGER source_events_no_update BEFORE UPDATE ON source_events
BEGIN SELECT RAISE(ABORT, 'source events are immutable'); END;
CREATE TRIGGER source_events_no_delete BEFORE DELETE ON source_events
BEGIN SELECT RAISE(ABORT, 'source events are immutable'); END;

CREATE TABLE import_documents (
  id INTEGER PRIMARY KEY,
  institution TEXT NOT NULL,
  sha256 TEXT NOT NULL,
  contents BLOB NOT NULL,
  imported_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  UNIQUE (institution, sha256)
) STRICT;
CREATE TABLE import_document_events (
  document_id INTEGER NOT NULL REFERENCES import_documents(id),
  event_id INTEGER NOT NULL REFERENCES source_events(id),
  PRIMARY KEY (document_id, event_id)
) STRICT;
CREATE TRIGGER import_documents_no_update BEFORE UPDATE ON import_documents
BEGIN SELECT RAISE(ABORT, 'import documents are immutable'); END;
CREATE TRIGGER import_documents_no_delete BEFORE DELETE ON import_documents
BEGIN SELECT RAISE(ABORT, 'import documents are immutable'); END;
PRAGMA user_version = 2;
|}
;;

let upgrade_v1 =
  {|CREATE TABLE source_events_v2 (
  id INTEGER PRIMARY KEY,
  venue TEXT NOT NULL,
  account TEXT NOT NULL,
  kind TEXT NOT NULL,
  source_id TEXT NOT NULL,
  occurred_at_ms INTEGER CHECK (occurred_at_ms >= 0),
  occurred_on TEXT,
  retrieved_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  payload BLOB NOT NULL,
  CHECK ((occurred_at_ms IS NOT NULL) != (occurred_on IS NOT NULL)),
  UNIQUE (venue, account, kind, source_id)
) STRICT;
CREATE TABLE journal_entries_v2 (
  id INTEGER PRIMARY KEY,
  event_id INTEGER NOT NULL REFERENCES source_events_v2(id),
  projection_key TEXT NOT NULL,
  date TEXT NOT NULL,
  description TEXT NOT NULL,
  UNIQUE (event_id, projection_key)
) STRICT;
CREATE TABLE postings_v2 (
  entry_id INTEGER NOT NULL REFERENCES journal_entries_v2(id),
  ordinal INTEGER NOT NULL CHECK (ordinal >= 0),
  account TEXT NOT NULL,
  commodity TEXT NOT NULL,
  quantity TEXT NOT NULL,
  PRIMARY KEY (entry_id, ordinal)
) STRICT;

INSERT INTO source_events_v2(id,venue,account,kind,source_id,occurred_at_ms,retrieved_at,payload)
SELECT id,venue,account,kind,source_id,occurred_at_ms,retrieved_at,payload FROM source_events;
INSERT INTO journal_entries_v2 SELECT * FROM journal_entries;
INSERT INTO postings_v2 SELECT * FROM postings;
DROP TABLE postings;
DROP TABLE journal_entries;
DROP TABLE source_events;
ALTER TABLE source_events_v2 RENAME TO source_events;
ALTER TABLE journal_entries_v2 RENAME TO journal_entries;
ALTER TABLE postings_v2 RENAME TO postings;
CREATE INDEX source_events_time ON source_events (occurred_at_ms, id);
CREATE INDEX journal_entries_date ON journal_entries (date, event_id, projection_key);
CREATE TRIGGER source_events_no_update BEFORE UPDATE ON source_events
BEGIN SELECT RAISE(ABORT, 'source events are immutable'); END;
CREATE TRIGGER source_events_no_delete BEFORE DELETE ON source_events
BEGIN SELECT RAISE(ABORT, 'source events are immutable'); END;


CREATE TABLE import_documents (
  id INTEGER PRIMARY KEY,
  institution TEXT NOT NULL,
  sha256 TEXT NOT NULL,
  contents BLOB NOT NULL,
  imported_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  UNIQUE (institution, sha256)
) STRICT;
CREATE TABLE import_document_events (
  document_id INTEGER NOT NULL REFERENCES import_documents(id),
  event_id INTEGER NOT NULL REFERENCES source_events(id),
  PRIMARY KEY (document_id, event_id)
) STRICT;
CREATE TRIGGER import_documents_no_update BEFORE UPDATE ON import_documents
BEGIN SELECT RAISE(ABORT, 'import documents are immutable'); END;
CREATE TRIGGER import_documents_no_delete BEFORE DELETE ON import_documents
BEGIN SELECT RAISE(ABORT, 'import documents are immutable'); END;

PRAGMA user_version = 2;
|}
;;

let upgrade_v2 =
  {|
CREATE TABLE bank_import_attempts (
  id INTEGER PRIMARY KEY,
  institution TEXT NOT NULL,
  path TEXT NOT NULL,
  document_id INTEGER REFERENCES import_documents(id),
  format TEXT NOT NULL CHECK (format IN ('ofx','csv','qif','spreadsheet','unknown')),
  outcome TEXT NOT NULL CHECK (outcome IN ('imported','unchanged','rejected','unsupported','unreadable')),
  inserted_count INTEGER NOT NULL CHECK (inserted_count >= 0),
  existing_count INTEGER NOT NULL CHECK (existing_count >= 0),
  error TEXT,
  attempted_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  CHECK ((outcome = 'unreadable') = (document_id IS NULL))
) STRICT;
CREATE INDEX bank_import_attempts_document ON bank_import_attempts(document_id,id);
CREATE TRIGGER bank_import_attempts_no_update BEFORE UPDATE ON bank_import_attempts
BEGIN SELECT RAISE(ABORT, 'import history is append-only'); END;
CREATE TRIGGER bank_import_attempts_no_delete BEFORE DELETE ON bank_import_attempts
BEGIN SELECT RAISE(ABORT, 'import history is append-only'); END;
CREATE TABLE bank_account_mappings (
  institution TEXT NOT NULL,
  source_account TEXT NOT NULL,
  ledger_account TEXT NOT NULL,
  PRIMARY KEY (institution, source_account)
) STRICT;
CREATE TABLE bank_account_mapping_changes (
  id INTEGER PRIMARY KEY,
  institution TEXT NOT NULL,
  source_account TEXT NOT NULL,
  previous_ledger_account TEXT,
  ledger_account TEXT NOT NULL,
  changed_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now'))
) STRICT;
CREATE TRIGGER bank_account_mapping_changes_no_update BEFORE UPDATE ON bank_account_mapping_changes
BEGIN SELECT RAISE(ABORT, 'mapping history is append-only'); END;
CREATE TRIGGER bank_account_mapping_changes_no_delete BEFORE DELETE ON bank_account_mapping_changes
BEGIN SELECT RAISE(ABORT, 'mapping history is append-only'); END;
PRAGMA user_version = 3;
|}
;;

let upgrade_v3 =
  {|
CREATE TABLE bank_csv_rows (
  id INTEGER PRIMARY KEY,
  document_id INTEGER NOT NULL REFERENCES import_documents(id),
  row_number INTEGER NOT NULL CHECK(row_number > 0),
  institution TEXT NOT NULL,
  account_id TEXT NOT NULL,
  booked_on TEXT NOT NULL,
  value_on TEXT,
  amount TEXT NOT NULL,
  description TEXT NOT NULL,
  suggested_description TEXT,
  category TEXT,
  parent_category TEXT,
  comment TEXT,
  account_label TEXT,
  reported_balance TEXT,
  pointage TEXT,
  fields_json TEXT NOT NULL,
  UNIQUE(document_id,row_number)
) STRICT;
CREATE INDEX bank_csv_rows_account_date ON bank_csv_rows(institution,account_id,booked_on);
CREATE TRIGGER bank_csv_rows_no_update BEFORE UPDATE ON bank_csv_rows
BEGIN SELECT RAISE(ABORT, 'CSV observations are immutable'); END;
CREATE TRIGGER bank_csv_rows_no_delete BEFORE DELETE ON bank_csv_rows
BEGIN SELECT RAISE(ABORT, 'CSV observations are immutable'); END;
CREATE TABLE bank_csv_matches (
  row_id INTEGER PRIMARY KEY REFERENCES bank_csv_rows(id),
  event_id INTEGER REFERENCES source_events(id),
  state TEXT NOT NULL CHECK(state IN ('matched','unmatched','ambiguous','conflict')),
  reason TEXT NOT NULL,
  candidates_json TEXT NOT NULL,
  policy TEXT NOT NULL,
  updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  CHECK ((state IN ('matched','conflict')) = (event_id IS NOT NULL))
) STRICT;
CREATE TABLE bank_csv_match_changes (
  id INTEGER PRIMARY KEY,
  row_id INTEGER NOT NULL REFERENCES bank_csv_rows(id),
  previous_event_id INTEGER REFERENCES source_events(id),
  event_id INTEGER REFERENCES source_events(id),
  previous_state TEXT,
  state TEXT NOT NULL,
  reason TEXT NOT NULL,
  candidates_json TEXT NOT NULL,
  policy TEXT NOT NULL,
  changed_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now'))
) STRICT;
CREATE TRIGGER bank_csv_match_changes_no_update BEFORE UPDATE ON bank_csv_match_changes
BEGIN SELECT RAISE(ABORT, 'CSV matching history is append-only'); END;
CREATE TRIGGER bank_csv_match_changes_no_delete BEFORE DELETE ON bank_csv_match_changes
BEGIN SELECT RAISE(ABORT, 'CSV matching history is append-only'); END;
PRAGMA user_version=4;
|}
;;

let upgrade_v4 =
  {|
CREATE TABLE bank_import_attempts_v5 (
  id INTEGER PRIMARY KEY,
  institution TEXT NOT NULL,
  path TEXT NOT NULL,
  document_id INTEGER REFERENCES import_documents(id),
  format TEXT NOT NULL CHECK (format IN ('ofx','csv','qif','spreadsheet','unknown','camt053','wise_csv')),
  outcome TEXT NOT NULL CHECK (outcome IN ('imported','unchanged','rejected','unsupported','unreadable')),
  inserted_count INTEGER NOT NULL CHECK (inserted_count >= 0),
  existing_count INTEGER NOT NULL CHECK (existing_count >= 0),
  error TEXT,
  attempted_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  CHECK ((outcome = 'unreadable') = (document_id IS NULL))
) STRICT;
CREATE INDEX bank_import_attempts_v5_document ON bank_import_attempts_v5(document_id,id);
CREATE TRIGGER bank_import_attempts_v5_no_update BEFORE UPDATE ON bank_import_attempts_v5
BEGIN SELECT RAISE(ABORT, 'import history is append-only'); END;
CREATE TRIGGER bank_import_attempts_v5_no_delete BEFORE DELETE ON bank_import_attempts_v5
BEGIN SELECT RAISE(ABORT, 'import history is append-only'); END;

INSERT INTO bank_import_attempts_v5 SELECT * FROM bank_import_attempts;
DROP TABLE bank_import_attempts;
ALTER TABLE bank_import_attempts_v5 RENAME TO bank_import_attempts;

CREATE TABLE wise_statements (
  id INTEGER PRIMARY KEY,
  document_id INTEGER NOT NULL REFERENCES import_documents(id),
  ordinal INTEGER NOT NULL,
  institution TEXT NOT NULL,
  source_account TEXT NOT NULL,
  statement_reference TEXT NOT NULL,
  currency TEXT NOT NULL,
  period_start TEXT NOT NULL,
  period_end TEXT NOT NULL,
  opening_at TEXT NOT NULL,
  closing_at TEXT NOT NULL,
  opening TEXT NOT NULL,
  closing TEXT NOT NULL,
  UNIQUE(document_id,ordinal)
) STRICT;
CREATE TABLE wise_statement_events (
  statement_id INTEGER NOT NULL REFERENCES wise_statements(id),
  event_id INTEGER NOT NULL REFERENCES source_events(id),
  PRIMARY KEY(statement_id,event_id)
) STRICT;
CREATE TRIGGER wise_statements_no_update BEFORE UPDATE ON wise_statements
BEGIN SELECT RAISE(ABORT, 'Wise statements are immutable'); END;
CREATE TRIGGER wise_statements_no_delete BEFORE DELETE ON wise_statements
BEGIN SELECT RAISE(ABORT, 'Wise statements are immutable'); END;
CREATE TABLE wise_csv_rows (
  id INTEGER PRIMARY KEY,
  document_id INTEGER NOT NULL REFERENCES import_documents(id),
  row_number INTEGER NOT NULL CHECK(row_number>0),
  institution TEXT NOT NULL,
  fields_json TEXT NOT NULL,
  UNIQUE(document_id,row_number)
) STRICT;
CREATE TRIGGER wise_csv_rows_no_update BEFORE UPDATE ON wise_csv_rows
BEGIN SELECT RAISE(ABORT, 'Wise CSV observations are immutable'); END;
CREATE TRIGGER wise_csv_rows_no_delete BEFORE DELETE ON wise_csv_rows
BEGIN SELECT RAISE(ABORT, 'Wise CSV observations are immutable'); END;
CREATE TABLE wise_csv_matches (
  row_id INTEGER PRIMARY KEY REFERENCES wise_csv_rows(id),
  event_id INTEGER REFERENCES source_events(id),
  state TEXT NOT NULL CHECK(state IN ('matched','unmatched','ambiguous','conflict')),
  reason TEXT NOT NULL,
  candidates_json TEXT NOT NULL,
  policy TEXT NOT NULL,
  updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  CHECK ((state IN ('matched','conflict')) = (event_id IS NOT NULL))
) STRICT;
CREATE TABLE wise_csv_match_changes (
  id INTEGER PRIMARY KEY,
  row_id INTEGER NOT NULL REFERENCES wise_csv_rows(id),
  previous_event_id INTEGER REFERENCES source_events(id),
  event_id INTEGER REFERENCES source_events(id),
  previous_state TEXT,
  state TEXT NOT NULL,
  reason TEXT NOT NULL,
  candidates_json TEXT NOT NULL,
  policy TEXT NOT NULL,
  changed_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now'))
) STRICT;
CREATE TRIGGER wise_csv_match_changes_no_update BEFORE UPDATE ON wise_csv_match_changes
BEGIN SELECT RAISE(ABORT, 'Wise matching history is append-only'); END;
CREATE TRIGGER wise_csv_match_changes_no_delete BEFORE DELETE ON wise_csv_match_changes
BEGIN SELECT RAISE(ABORT, 'Wise matching history is append-only'); END;
PRAGMA user_version=5;
|}
;;

let upgrade_v5 =
  {|
CREATE TABLE bank_categories (
  event_id INTEGER PRIMARY KEY REFERENCES source_events(id),
  account TEXT NOT NULL,
  reason TEXT NOT NULL
) STRICT;
CREATE TABLE bank_category_changes (
  id INTEGER PRIMARY KEY,
  event_id INTEGER NOT NULL REFERENCES source_events(id),
  previous_account TEXT,
  account TEXT NOT NULL,
  reason TEXT NOT NULL,
  changed_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now'))
) STRICT;
CREATE TRIGGER bank_category_changes_no_update BEFORE UPDATE ON bank_category_changes
BEGIN SELECT RAISE(ABORT, 'category history is append-only'); END;
CREATE TRIGGER bank_category_changes_no_delete BEFORE DELETE ON bank_category_changes
BEGIN SELECT RAISE(ABORT, 'category history is append-only'); END;
PRAGMA user_version=6;
|}
;;

let upgrade_v6 =
  {|

CREATE TABLE bank_import_attempts_v7 (
  id INTEGER PRIMARY KEY,
  institution TEXT NOT NULL,
  path TEXT NOT NULL,
  document_id INTEGER REFERENCES import_documents(id),
  format TEXT NOT NULL CHECK (format IN ('ofx','csv','qif','spreadsheet','unknown','camt053','wise_csv','bunq_mt940','bunq_csv')),
  outcome TEXT NOT NULL CHECK (outcome IN ('imported','unchanged','rejected','unsupported','unreadable')),
  inserted_count INTEGER NOT NULL CHECK (inserted_count >= 0),
  existing_count INTEGER NOT NULL CHECK (existing_count >= 0),
  error TEXT,
  attempted_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  CHECK ((outcome = 'unreadable') = (document_id IS NULL))
) STRICT;
CREATE INDEX bank_import_attempts_v7_document ON bank_import_attempts_v7(document_id,id);
CREATE TRIGGER bank_import_attempts_v7_no_update BEFORE UPDATE ON bank_import_attempts_v7
BEGIN SELECT RAISE(ABORT, 'import history is append-only'); END;
CREATE TRIGGER bank_import_attempts_v7_no_delete BEFORE DELETE ON bank_import_attempts_v7
BEGIN SELECT RAISE(ABORT, 'import history is append-only'); END;

INSERT INTO bank_import_attempts_v7 SELECT * FROM bank_import_attempts;
DROP TABLE bank_import_attempts;
ALTER TABLE bank_import_attempts_v7 RENAME TO bank_import_attempts;


CREATE TABLE bunq_day_inventories (
  source_account TEXT NOT NULL,
  booked_on TEXT NOT NULL,
  signature TEXT NOT NULL,
  document_id INTEGER NOT NULL REFERENCES import_documents(id),
  PRIMARY KEY(source_account,booked_on)
) STRICT;
CREATE TRIGGER bunq_day_inventories_no_update BEFORE UPDATE ON bunq_day_inventories
BEGIN SELECT RAISE(ABORT, 'Bunq daily inventories are immutable'); END;
CREATE TRIGGER bunq_day_inventories_no_delete BEFORE DELETE ON bunq_day_inventories
BEGIN SELECT RAISE(ABORT, 'Bunq daily inventories are immutable'); END;
CREATE TABLE bunq_statements (
  document_id INTEGER PRIMARY KEY REFERENCES import_documents(id),
  account_id TEXT NOT NULL,
  currency TEXT NOT NULL,
  opening_on TEXT NOT NULL,
  closing_on TEXT NOT NULL,
  opening TEXT NOT NULL,
  closing TEXT NOT NULL
) STRICT;
CREATE TRIGGER bunq_statements_no_update BEFORE UPDATE ON bunq_statements
BEGIN SELECT RAISE(ABORT, 'Bunq statements are immutable'); END;
CREATE TRIGGER bunq_statements_no_delete BEFORE DELETE ON bunq_statements
BEGIN SELECT RAISE(ABORT, 'Bunq statements are immutable'); END;
CREATE TABLE bunq_csv_rows (
  id INTEGER PRIMARY KEY,
  document_id INTEGER NOT NULL REFERENCES import_documents(id),
  row_number INTEGER NOT NULL CHECK(row_number>0),
  institution TEXT NOT NULL,
  fields_json TEXT NOT NULL,
  UNIQUE(document_id,row_number)
) STRICT;
CREATE TRIGGER bunq_csv_rows_no_update BEFORE UPDATE ON bunq_csv_rows
BEGIN SELECT RAISE(ABORT, 'Bunq CSV observations are immutable'); END;
CREATE TRIGGER bunq_csv_rows_no_delete BEFORE DELETE ON bunq_csv_rows
BEGIN SELECT RAISE(ABORT, 'Bunq CSV observations are immutable'); END;
CREATE TABLE bunq_csv_matches (
  row_id INTEGER PRIMARY KEY REFERENCES bunq_csv_rows(id),
  event_id INTEGER REFERENCES source_events(id),
  state TEXT NOT NULL CHECK(state IN ('matched','unmatched','ambiguous','conflict')),
  reason TEXT NOT NULL,
  candidates_json TEXT NOT NULL,
  policy TEXT NOT NULL,
  updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  CHECK ((state IN ('matched','conflict')) = (event_id IS NOT NULL))
) STRICT;
CREATE TABLE bunq_csv_match_changes (
  id INTEGER PRIMARY KEY,
  row_id INTEGER NOT NULL REFERENCES bunq_csv_rows(id),
  previous_event_id INTEGER REFERENCES source_events(id),
  event_id INTEGER REFERENCES source_events(id),
  previous_state TEXT,
  state TEXT NOT NULL,
  reason TEXT NOT NULL,
  candidates_json TEXT NOT NULL,
  policy TEXT NOT NULL,
  changed_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now'))
) STRICT;
CREATE TRIGGER bunq_csv_match_changes_no_update BEFORE UPDATE ON bunq_csv_match_changes
BEGIN SELECT RAISE(ABORT, 'Bunq matching history is append-only'); END;
CREATE TRIGGER bunq_csv_match_changes_no_delete BEFORE DELETE ON bunq_csv_match_changes
BEGIN SELECT RAISE(ABORT, 'Bunq matching history is append-only'); END;
PRAGMA user_version=7;

|}
;;

let upgrade_v7 =
  {|


CREATE TABLE bank_import_attempts_v8 (
  id INTEGER PRIMARY KEY,
  institution TEXT NOT NULL,
  path TEXT NOT NULL,
  document_id INTEGER REFERENCES import_documents(id),
  format TEXT NOT NULL CHECK (format IN ('ofx','csv','qif','spreadsheet','unknown','camt053','wise_csv','bunq_mt940','bunq_csv','revolut_csv')),
  outcome TEXT NOT NULL CHECK (outcome IN ('imported','unchanged','rejected','unsupported','unreadable')),
  inserted_count INTEGER NOT NULL CHECK (inserted_count >= 0),
  existing_count INTEGER NOT NULL CHECK (existing_count >= 0),
  error TEXT,
  attempted_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  CHECK ((outcome = 'unreadable') = (document_id IS NULL))
) STRICT;
CREATE INDEX bank_import_attempts_v8_document ON bank_import_attempts_v8(document_id,id);
CREATE TRIGGER bank_import_attempts_v8_no_update BEFORE UPDATE ON bank_import_attempts_v8
BEGIN SELECT RAISE(ABORT, 'import history is append-only'); END;
CREATE TRIGGER bank_import_attempts_v8_no_delete BEFORE DELETE ON bank_import_attempts_v8
BEGIN SELECT RAISE(ABORT, 'import history is append-only'); END;

INSERT INTO bank_import_attempts_v8 SELECT * FROM bank_import_attempts;
DROP TABLE bank_import_attempts;
ALTER TABLE bank_import_attempts_v8 RENAME TO bank_import_attempts;



CREATE TABLE revolut_documents (
  document_id INTEGER PRIMARY KEY REFERENCES import_documents(id),
  account_id TEXT NOT NULL
) STRICT;
CREATE TRIGGER revolut_documents_no_update BEFORE UPDATE ON revolut_documents
BEGIN SELECT RAISE(ABORT, 'Revolut document context is immutable'); END;
CREATE TRIGGER revolut_documents_no_delete BEFORE DELETE ON revolut_documents
BEGIN SELECT RAISE(ABORT, 'Revolut document context is immutable'); END;
CREATE TABLE revolut_day_inventories (
  source_account TEXT NOT NULL,
  booked_on TEXT NOT NULL,
  signature TEXT NOT NULL,
  document_id INTEGER NOT NULL REFERENCES import_documents(id),
  PRIMARY KEY(source_account,booked_on)
) STRICT;
CREATE TRIGGER revolut_day_inventories_no_update BEFORE UPDATE ON revolut_day_inventories
BEGIN SELECT RAISE(ABORT, 'Revolut day inventories are immutable'); END;
CREATE TRIGGER revolut_day_inventories_no_delete BEFORE DELETE ON revolut_day_inventories
BEGIN SELECT RAISE(ABORT, 'Revolut day inventories are immutable'); END;
CREATE TABLE bank_transfer_decisions (
  first_event_id INTEGER NOT NULL REFERENCES source_events(id),
  second_event_id INTEGER NOT NULL REFERENCES source_events(id),
  state TEXT NOT NULL CHECK(state IN ('confirmed','rejected')),
  reason TEXT NOT NULL,
  PRIMARY KEY(first_event_id,second_event_id),
  CHECK(first_event_id < second_event_id)
) STRICT;
CREATE TABLE bank_transfer_memberships (
  event_id INTEGER PRIMARY KEY REFERENCES source_events(id),
  first_event_id INTEGER NOT NULL,
  second_event_id INTEGER NOT NULL,
  FOREIGN KEY(first_event_id,second_event_id) REFERENCES bank_transfer_decisions(first_event_id,second_event_id),
  CHECK(event_id=first_event_id OR event_id=second_event_id)
) STRICT;
CREATE TABLE bank_transfer_changes (
  id INTEGER PRIMARY KEY,
  first_event_id INTEGER NOT NULL REFERENCES source_events(id),
  second_event_id INTEGER NOT NULL REFERENCES source_events(id),
  previous_state TEXT,
  state TEXT NOT NULL CHECK(state IN ('confirmed','rejected')),
  reason TEXT NOT NULL,
  changed_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now'))
) STRICT;
CREATE TRIGGER bank_transfer_changes_no_update BEFORE UPDATE ON bank_transfer_changes
BEGIN SELECT RAISE(ABORT, 'Transfer history is append-only'); END;
CREATE TRIGGER bank_transfer_changes_no_delete BEFORE DELETE ON bank_transfer_changes
BEGIN SELECT RAISE(ABORT, 'Transfer history is append-only'); END;
PRAGMA user_version=8;
|}
;;

let upgrade_v8 =
  {|
CREATE TABLE bank_account_identities (
  institution TEXT NOT NULL,
  source_account TEXT NOT NULL,
  iban TEXT NOT NULL,
  PRIMARY KEY(institution,source_account)
) STRICT;
CREATE TABLE bank_account_identity_changes (
  id INTEGER PRIMARY KEY,
  institution TEXT NOT NULL,
  source_account TEXT NOT NULL,
  previous_iban TEXT,
  iban TEXT NOT NULL,
  changed_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now'))
) STRICT;
CREATE TRIGGER bank_account_identity_changes_no_update BEFORE UPDATE ON bank_account_identity_changes
BEGIN SELECT RAISE(ABORT, 'Account identity history is append-only'); END;
CREATE TRIGGER bank_account_identity_changes_no_delete BEFORE DELETE ON bank_account_identity_changes
BEGIN SELECT RAISE(ABORT, 'Account identity history is append-only'); END;
PRAGMA user_version=9;
|}
;;

let upgrade_v9 =
  {|
CREATE TABLE bank_account_identity_changes_v10 (
  id INTEGER PRIMARY KEY,
  institution TEXT NOT NULL,
  source_account TEXT NOT NULL,
  previous_iban TEXT,
  iban TEXT,
  changed_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now'))
) STRICT;
INSERT INTO bank_account_identity_changes_v10 SELECT * FROM bank_account_identity_changes;
DROP TABLE bank_account_identity_changes;
ALTER TABLE bank_account_identity_changes_v10 RENAME TO bank_account_identity_changes;
CREATE TRIGGER bank_account_identity_changes_no_update BEFORE UPDATE ON bank_account_identity_changes
BEGIN SELECT RAISE(ABORT, 'Account identity history is append-only'); END;
CREATE TRIGGER bank_account_identity_changes_no_delete BEFORE DELETE ON bank_account_identity_changes
BEGIN SELECT RAISE(ABORT, 'Account identity history is append-only'); END;
CREATE TABLE bank_account_settlements (
  institution TEXT NOT NULL,
  source_account TEXT NOT NULL,
  settlement_account TEXT NOT NULL,
  PRIMARY KEY(institution,source_account)
) STRICT;
CREATE TABLE bank_account_settlement_changes (
  id INTEGER PRIMARY KEY,
  institution TEXT NOT NULL,
  source_account TEXT NOT NULL,
  previous_settlement TEXT,
  settlement_account TEXT,
  changed_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now'))
) STRICT;
CREATE TRIGGER bank_account_settlement_changes_no_update BEFORE UPDATE ON bank_account_settlement_changes
BEGIN SELECT RAISE(ABORT, 'Account settlement history is append-only'); END;
CREATE TRIGGER bank_account_settlement_changes_no_delete BEFORE DELETE ON bank_account_settlement_changes
BEGIN SELECT RAISE(ABORT, 'Account settlement history is append-only'); END;
PRAGMA user_version=10;
|}
;;
