CREATE TABLE source_events (
  id INTEGER PRIMARY KEY,
  venue TEXT NOT NULL,
  account TEXT NOT NULL,
  kind TEXT NOT NULL,
  source_id TEXT NOT NULL,
  occurred_at_ms INTEGER NOT NULL CHECK (occurred_at_ms >= 0),
  retrieved_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  payload BLOB NOT NULL,
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
PRAGMA user_version = 1;
