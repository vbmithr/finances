-- Removes the account in deleted_account and what account_deletion_scope gathered. A script;
-- the caller lifts the immutability triggers around it.
DELETE FROM postings WHERE entry_id IN
  (SELECT id FROM journal_entries WHERE event_id IN (SELECT id FROM deleted_events));
DELETE FROM journal_entries WHERE event_id IN (SELECT id FROM deleted_events);
DELETE FROM bank_categories WHERE event_id IN (SELECT id FROM deleted_events);
DELETE FROM bank_category_changes WHERE event_id IN (SELECT id FROM deleted_events);
DELETE FROM movement_type_overrides WHERE event_id IN (SELECT id FROM deleted_events);
DELETE FROM movement_type_changes WHERE event_id IN (SELECT id FROM deleted_events);
DELETE FROM operation_exchange_changes WHERE operation_id IN
  (SELECT id FROM deleted_operations);
DELETE FROM operation_exchange_decisions WHERE operation_id IN
  (SELECT id FROM deleted_operations);
DELETE FROM operation_resolution_changes WHERE movement_id IN
  (SELECT id FROM operation_movements
   WHERE operation_id IN (SELECT id FROM deleted_operations));
DELETE FROM operation_movement_resolutions WHERE movement_id IN
  (SELECT id FROM operation_movements
   WHERE operation_id IN (SELECT id FROM deleted_operations));
DELETE FROM operation_movement_sources WHERE movement_id IN
  (SELECT id FROM operation_movements
   WHERE operation_id IN (SELECT id FROM deleted_operations));
DELETE FROM operation_movements WHERE operation_id IN (SELECT id FROM deleted_operations);
DELETE FROM operation_sources WHERE operation_id IN (SELECT id FROM deleted_operations);
DELETE FROM operations WHERE id IN (SELECT id FROM deleted_operations);
DELETE FROM reversal_changes WHERE original IN (SELECT id FROM deleted_events)
  OR reversal IN (SELECT id FROM deleted_events);
DELETE FROM statement_events WHERE event_id IN (SELECT id FROM deleted_events)
  OR statement_id IN
    (SELECT id FROM statements WHERE document_id IN (SELECT id FROM deleted_documents));
DELETE FROM statements WHERE document_id IN (SELECT id FROM deleted_documents);
DELETE FROM stated_balances WHERE document_id IN (SELECT id FROM deleted_documents);
DELETE FROM import_document_events WHERE event_id IN (SELECT id FROM deleted_events);
DELETE FROM day_inventories WHERE (institution, source_account) IN
  (SELECT institution, source_account FROM deleted_account);
DELETE FROM bank_import_attempts WHERE document_id IN (SELECT id FROM deleted_documents);
DELETE FROM document_contexts WHERE document_id IN (SELECT id FROM deleted_documents);
DELETE FROM import_documents WHERE id IN (SELECT id FROM deleted_documents);
DELETE FROM movement_evidence WHERE event_id IN (SELECT id FROM deleted_events);
DELETE FROM movements WHERE event_id IN (SELECT id FROM deleted_events);
DELETE FROM source_event_fields WHERE event_id IN (SELECT id FROM deleted_events);
DELETE FROM source_events WHERE id IN (SELECT id FROM deleted_events);
DELETE FROM bank_account_mappings WHERE (institution, source_account) IN
  (SELECT institution, source_account FROM deleted_account);
DELETE FROM bank_account_mapping_changes WHERE (institution, source_account) IN
  (SELECT institution, source_account FROM deleted_account);
DELETE FROM bank_account_identities WHERE (institution, source_account) IN
  (SELECT institution, source_account FROM deleted_account);
DELETE FROM bank_account_identity_changes WHERE (institution, source_account) IN
  (SELECT institution, source_account FROM deleted_account);
DELETE FROM bank_account_settlements WHERE (institution, source_account) IN
  (SELECT institution, source_account FROM deleted_account);
DELETE FROM bank_account_settlement_changes WHERE (institution, source_account) IN
  (SELECT institution, source_account FROM deleted_account);
