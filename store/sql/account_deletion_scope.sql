-- What deleting the account in deleted_account removes: its source events, the documents
-- that carried them and the operations built from them. A script, run before any check.
DELETE FROM deleted_events;
DELETE FROM deleted_documents;
DELETE FROM deleted_operations;
INSERT INTO deleted_events
SELECT e.id FROM source_events e JOIN deleted_account a
  ON e.venue = a.institution AND e.account = a.source_account;
INSERT INTO deleted_documents
SELECT DISTINCT document_id FROM import_document_events
WHERE event_id IN (SELECT id FROM deleted_events);
INSERT INTO deleted_operations
SELECT DISTINCT operation_id FROM operation_sources
WHERE event_id IN (SELECT id FROM deleted_events);
