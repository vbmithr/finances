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
