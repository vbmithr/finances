-- The scratch tables account deletion fills and reads, emptied: the account to delete, then
-- what account_deletion_scope gathers for it. A script, run first.
CREATE TEMP TABLE IF NOT EXISTS deleted_account(
  institution TEXT NOT NULL, source_account TEXT NOT NULL, settlement_target TEXT NOT NULL);
CREATE TEMP TABLE IF NOT EXISTS deleted_events(id INTEGER PRIMARY KEY);
CREATE TEMP TABLE IF NOT EXISTS deleted_documents(id INTEGER PRIMARY KEY);
CREATE TEMP TABLE IF NOT EXISTS deleted_operations(id INTEGER PRIMARY KEY);
DELETE FROM deleted_account;
DELETE FROM deleted_events;
DELETE FROM deleted_documents;
DELETE FROM deleted_operations;
