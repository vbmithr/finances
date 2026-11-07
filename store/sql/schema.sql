-- Generated from store/db_schema.ml by store/schema: do not edit. The tables are
-- defined in OCaml; dune runtest checks this copy and dune promote refreshes it.

CREATE TABLE "source_events" (
  "id" INTEGER NOT NULL,
  "venue" TEXT NOT NULL,
  "account" TEXT NOT NULL,
  "kind" TEXT NOT NULL,
  "source_id" TEXT NOT NULL,
  "occurred_at" TEXT,
  "occurred_on" TEXT,
  "retrieved_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  "payload" BLOB NOT NULL,
  PRIMARY KEY ("id"),
  UNIQUE ("venue", "account", "kind", "source_id"),
  CHECK ("occurred_at" IS NULL OR (length("occurred_at") = 33 AND "occurred_at" GLOB '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]Z')),
  CHECK (("occurred_at" IS NOT NULL) != ("occurred_on" IS NOT NULL))
) STRICT;
CREATE INDEX "source_events_time" ON "source_events" ("occurred_at", "id");
CREATE INDEX "source_events_occurrence" ON "source_events" ("venue",
                                                            "account",
                                                            "kind",
                                                            "occurred_on",
                                                            "occurred_at");
CREATE TRIGGER "source_events_no_update" BEFORE UPDATE ON "source_events" BEGIN SELECT RAISE(ABORT, 'source events are immutable'); END;
CREATE TRIGGER "source_events_no_delete" BEFORE DELETE ON "source_events" BEGIN SELECT RAISE(ABORT, 'source events are immutable'); END;
CREATE TABLE "movements" (
  "event_id" INTEGER NOT NULL,
  "venue" TEXT NOT NULL,
  "account" TEXT NOT NULL,
  "bank_id" TEXT NOT NULL,
  "account_id" TEXT NOT NULL,
  "account_type" TEXT NOT NULL,
  "currency" TEXT NOT NULL,
  "own_iban_status" TEXT NOT NULL,
  "own_iban" TEXT,
  "booked_on" TEXT NOT NULL,
  "value_on" TEXT,
  "amount" TEXT NOT NULL,
  "fee" TEXT NOT NULL,
  "principal_magnitude" TEXT NOT NULL,
  "principal_sign" TEXT NOT NULL,
  "counterparty_iban" TEXT,
  "descriptions" TEXT NOT NULL,
  "movement_type" TEXT,
  "main_description" INTEGER NOT NULL,
  PRIMARY KEY ("event_id"),
  FOREIGN KEY ("event_id") REFERENCES "source_events" ("id"),
  CHECK ("principal_sign" IN ('+','-')),
  CHECK ("own_iban_status" IN ('known','unknown','not_applicable')),
  CHECK (("own_iban_status" = 'known') = ("own_iban" IS NOT NULL)),
  CHECK ("main_description" >= 0)
) STRICT;
CREATE INDEX "movements_principal" ON "movements" ("currency",
                                                   "principal_magnitude");
CREATE INDEX "movements_day" ON "movements" ("currency", "booked_on");
CREATE TRIGGER "movements_no_update" BEFORE UPDATE OF "event_id", "venue", "account", "bank_id", "account_id", "account_type", "currency", "own_iban_status", "own_iban", "booked_on", "value_on", "amount", "fee", "principal_magnitude", "principal_sign", "counterparty_iban", "descriptions", "main_description" ON "movements" BEGIN SELECT RAISE(ABORT, 'movements are derived from immutable source events'); END;
CREATE TRIGGER "movements_no_delete" BEFORE DELETE ON "movements" BEGIN SELECT RAISE(ABORT, 'movements are derived from immutable source events'); END;
CREATE TABLE "operations" (
  "id" INTEGER NOT NULL,
  "occurred" TEXT NOT NULL,
  "bank_event_id" INTEGER,
  "source_event_id" INTEGER,
  PRIMARY KEY ("id"),
  UNIQUE ("bank_event_id"),
  UNIQUE ("source_event_id"),
  FOREIGN KEY ("bank_event_id") REFERENCES "source_events" ("id"),
  FOREIGN KEY ("source_event_id") REFERENCES "source_events" ("id")
) STRICT;
CREATE TABLE "operation_sources" (
  "operation_id" INTEGER NOT NULL,
  "event_id" INTEGER NOT NULL,
  PRIMARY KEY ("operation_id", "event_id"),
  FOREIGN KEY ("operation_id") REFERENCES "operations" ("id"),
  FOREIGN KEY ("event_id") REFERENCES "source_events" ("id")
) STRICT;
CREATE INDEX "operation_sources_event" ON "operation_sources" ("event_id");
CREATE TABLE "operation_movements" (
  "id" INTEGER NOT NULL,
  "operation_id" INTEGER NOT NULL,
  "anchor_event_id" INTEGER NOT NULL,
  "role" TEXT NOT NULL,
  "account" TEXT,
  "commodity" TEXT NOT NULL,
  "quantity" TEXT NOT NULL,
  PRIMARY KEY ("id"),
  UNIQUE ("anchor_event_id", "role"),
  UNIQUE ("id", "operation_id"),
  UNIQUE ("id", "anchor_event_id"),
  FOREIGN KEY ("operation_id", "anchor_event_id") REFERENCES "operation_sources" ("operation_id", "event_id")
) STRICT;
CREATE INDEX "operation_movements_operation" ON "operation_movements" 
  ("operation_id");
CREATE TABLE "operation_movement_sources" (
  "movement_id" INTEGER NOT NULL,
  "operation_id" INTEGER NOT NULL,
  "event_id" INTEGER NOT NULL,
  PRIMARY KEY ("movement_id", "event_id"),
  FOREIGN KEY ("movement_id", "operation_id") REFERENCES "operation_movements" ("id", "operation_id"),
  FOREIGN KEY ("operation_id", "event_id") REFERENCES "operation_sources" ("operation_id", "event_id")
) STRICT;
CREATE TABLE "operation_transfer_links" (
  "first_movement_id" INTEGER NOT NULL,
  "second_movement_id" INTEGER NOT NULL,
  "reason" TEXT NOT NULL,
  PRIMARY KEY ("first_movement_id", "second_movement_id"),
  FOREIGN KEY ("first_movement_id") REFERENCES "operation_movements" ("id"),
  FOREIGN KEY ("second_movement_id") REFERENCES "operation_movements" ("id"),
  CHECK (("first_movement_id" = "second_movement_id") != ("first_movement_id" = "first_movement_id"))
) STRICT;
CREATE TABLE "operation_transfer_memberships" (
  "movement_id" INTEGER NOT NULL,
  "first_movement_id" INTEGER NOT NULL,
  "second_movement_id" INTEGER NOT NULL,
  PRIMARY KEY ("movement_id"),
  FOREIGN KEY ("movement_id") REFERENCES "operation_movements" ("id"),
  FOREIGN KEY ("first_movement_id", "second_movement_id") REFERENCES "operation_transfer_links" ("first_movement_id", "second_movement_id"),
  CHECK (("movement_id" = "first_movement_id") OR ("movement_id" = "second_movement_id"))
) STRICT;
CREATE TABLE "operation_movement_resolutions" (
  "movement_id" INTEGER NOT NULL,
  "account" TEXT NOT NULL,
  PRIMARY KEY ("movement_id"),
  FOREIGN KEY ("movement_id") REFERENCES "operation_movements" ("id")
) STRICT;
CREATE TABLE "operation_resolution_changes" (
  "id" INTEGER NOT NULL,
  "movement_id" INTEGER NOT NULL,
  "account" TEXT NOT NULL,
  "changed_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id"),
  FOREIGN KEY ("movement_id") REFERENCES "operation_movements" ("id")
) STRICT;
CREATE TRIGGER "operation_resolution_changes_no_update" BEFORE UPDATE ON "operation_resolution_changes" BEGIN SELECT RAISE(ABORT, 'movement resolution history is append-only'); END;
CREATE TRIGGER "operation_resolution_changes_no_delete" BEFORE DELETE ON "operation_resolution_changes" BEGIN SELECT RAISE(ABORT, 'movement resolution history is append-only'); END;
CREATE TABLE "asset_mappings" (
  "provider" TEXT NOT NULL,
  "symbol" TEXT NOT NULL,
  "commodity" TEXT,
  PRIMARY KEY ("provider", "symbol")
) STRICT;
CREATE TABLE "asset_mapping_changes" (
  "id" INTEGER NOT NULL,
  "provider" TEXT NOT NULL,
  "symbol" TEXT NOT NULL,
  "previous" TEXT,
  "commodity" TEXT,
  "action" TEXT NOT NULL,
  "changed_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id"),
  CHECK ("action" IN ('mapped','ignored','removed'))
) STRICT;
CREATE TRIGGER "asset_mapping_changes_no_update" BEFORE UPDATE ON "asset_mapping_changes" BEGIN SELECT RAISE(ABORT, 'asset mapping history is append-only'); END;
CREATE TRIGGER "asset_mapping_changes_no_delete" BEFORE DELETE ON "asset_mapping_changes" BEGIN SELECT RAISE(ABORT, 'asset mapping history is append-only'); END;
CREATE TABLE "operation_exchange_decisions" (
  "operation_id" INTEGER NOT NULL,
  "reason" TEXT NOT NULL,
  PRIMARY KEY ("operation_id"),
  FOREIGN KEY ("operation_id") REFERENCES "operations" ("id")
) STRICT;
CREATE TABLE "operation_exchange_changes" (
  "id" INTEGER NOT NULL,
  "operation_id" INTEGER NOT NULL,
  "action" TEXT NOT NULL,
  "changed_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id"),
  FOREIGN KEY ("operation_id") REFERENCES "operations" ("id"),
  CHECK ("action" IN ('confirmed','undone'))
) STRICT;
CREATE TRIGGER "operation_exchange_changes_no_update" BEFORE UPDATE ON "operation_exchange_changes" BEGIN SELECT RAISE(ABORT, 'operation exchange history is append-only'); END;
CREATE TRIGGER "operation_exchange_changes_no_delete" BEFORE DELETE ON "operation_exchange_changes" BEGIN SELECT RAISE(ABORT, 'operation exchange history is append-only'); END;
CREATE TABLE "operation_transfer_changes" (
  "id" INTEGER NOT NULL,
  "first_movement_id" INTEGER NOT NULL,
  "second_movement_id" INTEGER NOT NULL,
  "action" TEXT NOT NULL,
  "changed_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id"),
  FOREIGN KEY ("first_movement_id") REFERENCES "operation_movements" ("id"),
  FOREIGN KEY ("second_movement_id") REFERENCES "operation_movements" ("id"),
  CHECK ("action" IN ('confirmed','undone'))
) STRICT;
CREATE TRIGGER "operation_transfer_changes_no_update" BEFORE UPDATE ON "operation_transfer_changes" BEGIN SELECT RAISE(ABORT, 'operation transfer history is append-only'); END;
CREATE TRIGGER "operation_transfer_changes_no_delete" BEFORE DELETE ON "operation_transfer_changes" BEGIN SELECT RAISE(ABORT, 'operation transfer history is append-only'); END;
CREATE TABLE "source_event_fields" (
  "event_id" INTEGER NOT NULL,
  "ordinal" INTEGER NOT NULL,
  "name" TEXT NOT NULL,
  "value" TEXT NOT NULL,
  PRIMARY KEY ("event_id", "ordinal"),
  FOREIGN KEY ("event_id") REFERENCES "source_events" ("id"),
  CHECK ("ordinal" >= 0)
) STRICT;
CREATE TABLE "source_event_digests" (
  "event_id" INTEGER NOT NULL,
  "payload_sha256" TEXT NOT NULL,
  "fields_sha256" TEXT NOT NULL,
  "evidence" TEXT NOT NULL,
  PRIMARY KEY ("event_id"),
  FOREIGN KEY ("event_id") REFERENCES "source_events" ("id")
) STRICT;
CREATE TABLE "movement_evidence" (
  "event_id" INTEGER NOT NULL,
  "card_reference" TEXT,
  "withdrawal_reversal" TEXT NOT NULL,
  PRIMARY KEY ("event_id"),
  FOREIGN KEY ("event_id") REFERENCES "source_events" ("id"),
  CHECK ("withdrawal_reversal" IN ('yes','no'))
) STRICT;
CREATE TABLE "journal_entries" (
  "id" INTEGER NOT NULL,
  "event_id" INTEGER NOT NULL,
  "projection_key" TEXT NOT NULL,
  "date" TEXT NOT NULL,
  "description" TEXT NOT NULL,
  PRIMARY KEY ("id"),
  UNIQUE ("event_id", "projection_key"),
  FOREIGN KEY ("event_id") REFERENCES "source_events" ("id")
) STRICT;
CREATE INDEX "journal_entries_date" ON "journal_entries" ("date", "event_id",
                                                          "projection_key");
CREATE TABLE "postings" (
  "entry_id" INTEGER NOT NULL,
  "ordinal" INTEGER NOT NULL,
  "account" TEXT NOT NULL,
  "commodity" TEXT NOT NULL,
  "quantity" TEXT NOT NULL,
  PRIMARY KEY ("entry_id", "ordinal"),
  FOREIGN KEY ("entry_id") REFERENCES "journal_entries" ("id"),
  CHECK ("ordinal" >= 0)
) STRICT;
CREATE TABLE "import_documents" (
  "id" INTEGER NOT NULL,
  "institution" TEXT NOT NULL,
  "sha256" TEXT NOT NULL,
  "contents" BLOB NOT NULL,
  "imported_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id"),
  UNIQUE ("institution", "sha256")
) STRICT;
CREATE TRIGGER "import_documents_no_update" BEFORE UPDATE ON "import_documents" BEGIN SELECT RAISE(ABORT, 'import documents are immutable'); END;
CREATE TRIGGER "import_documents_no_delete" BEFORE DELETE ON "import_documents" BEGIN SELECT RAISE(ABORT, 'import documents are immutable'); END;
CREATE TABLE "import_document_events" (
  "document_id" INTEGER NOT NULL,
  "event_id" INTEGER NOT NULL,
  PRIMARY KEY ("document_id", "event_id"),
  FOREIGN KEY ("event_id") REFERENCES "source_events" ("id"),
  FOREIGN KEY ("document_id") REFERENCES "import_documents" ("id")
) STRICT;
CREATE INDEX "import_document_events_event" ON "import_document_events" 
  ("event_id");
CREATE TABLE "bank_account_mappings" (
  "institution" TEXT NOT NULL,
  "source_account" TEXT NOT NULL,
  "ledger_account" TEXT NOT NULL,
  "public_id" TEXT NOT NULL,
  PRIMARY KEY ("institution", "source_account")
) STRICT;
CREATE TABLE "bank_account_mapping_changes" (
  "id" INTEGER NOT NULL,
  "institution" TEXT NOT NULL,
  "source_account" TEXT NOT NULL,
  "previous_ledger_account" TEXT,
  "ledger_account" TEXT NOT NULL,
  "changed_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id")
) STRICT;
CREATE TRIGGER "bank_account_mapping_changes_no_update" BEFORE UPDATE ON "bank_account_mapping_changes" BEGIN SELECT RAISE(ABORT, 'mapping history is append-only'); END;
CREATE TRIGGER "bank_account_mapping_changes_no_delete" BEFORE DELETE ON "bank_account_mapping_changes" BEGIN SELECT RAISE(ABORT, 'mapping history is append-only'); END;
CREATE TABLE "statements" (
  "id" INTEGER NOT NULL,
  "document_id" INTEGER NOT NULL,
  "ordinal" INTEGER NOT NULL,
  "institution" TEXT NOT NULL,
  "source_account" TEXT NOT NULL,
  "reference" TEXT,
  "currency" TEXT NOT NULL,
  "period_start" TEXT NOT NULL,
  "period_end" TEXT NOT NULL,
  "opening_as_of" TEXT NOT NULL,
  "closing_as_of" TEXT NOT NULL,
  "opening" TEXT NOT NULL,
  "closing" TEXT NOT NULL,
  PRIMARY KEY ("id"),
  UNIQUE ("document_id", "ordinal"),
  FOREIGN KEY ("document_id") REFERENCES "import_documents" ("id")
) STRICT;
CREATE TRIGGER "statements_no_update" BEFORE UPDATE ON "statements" BEGIN SELECT RAISE(ABORT, 'statements are immutable'); END;
CREATE TRIGGER "statements_no_delete" BEFORE DELETE ON "statements" BEGIN SELECT RAISE(ABORT, 'statements are immutable'); END;
CREATE TABLE "stated_balances" (
  "document_id" INTEGER NOT NULL,
  "institution" TEXT NOT NULL,
  "account" TEXT NOT NULL,
  "balance_type" TEXT NOT NULL,
  "commodity_json" TEXT NOT NULL,
  "quantity" TEXT NOT NULL,
  "as_of" TEXT NOT NULL,
  PRIMARY KEY ("document_id", "account", "balance_type", "commodity_json", "as_of"),
  FOREIGN KEY ("document_id") REFERENCES "import_documents" ("id"),
  CHECK ("balance_type" IN ('opening','closing','available','day_end'))
) STRICT;
CREATE TRIGGER "stated_balances_no_update" BEFORE UPDATE ON "stated_balances" BEGIN SELECT RAISE(ABORT, 'stated balances are immutable'); END;
CREATE TRIGGER "stated_balances_no_delete" BEFORE DELETE ON "stated_balances" BEGIN SELECT RAISE(ABORT, 'stated balances are immutable'); END;
CREATE TABLE "statement_events" (
  "statement_id" INTEGER NOT NULL,
  "event_id" INTEGER NOT NULL,
  PRIMARY KEY ("statement_id", "event_id"),
  FOREIGN KEY ("event_id") REFERENCES "source_events" ("id"),
  FOREIGN KEY ("statement_id") REFERENCES "statements" ("id")
) STRICT;
CREATE TABLE "companion_rows" (
  "id" INTEGER NOT NULL,
  "document_id" INTEGER NOT NULL,
  "row_number" INTEGER NOT NULL,
  "institution" TEXT NOT NULL,
  "format" TEXT NOT NULL,
  "fields_json" TEXT NOT NULL,
  "display_name" TEXT,
  PRIMARY KEY ("id"),
  UNIQUE ("document_id", "row_number"),
  FOREIGN KEY ("document_id") REFERENCES "import_documents" ("id"),
  CHECK ("row_number" > 0)
) STRICT;
CREATE INDEX "companion_rows_format" ON "companion_rows" ("institution",
                                                          "format");
CREATE TRIGGER "companion_rows_no_update" BEFORE UPDATE ON "companion_rows" BEGIN SELECT RAISE(ABORT, 'companion observations are immutable'); END;
CREATE TRIGGER "companion_rows_no_delete" BEFORE DELETE ON "companion_rows" BEGIN SELECT RAISE(ABORT, 'companion observations are immutable'); END;
CREATE TABLE "companion_matches" (
  "row_id" INTEGER NOT NULL,
  "event_id" INTEGER,
  "state" TEXT NOT NULL,
  "reason" TEXT NOT NULL,
  "candidates_json" TEXT NOT NULL,
  "policy" TEXT NOT NULL,
  "updated_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("row_id"),
  FOREIGN KEY ("event_id") REFERENCES "source_events" ("id"),
  FOREIGN KEY ("row_id") REFERENCES "companion_rows" ("id"),
  CHECK ("state" IN ('matched','unmatched','ambiguous','conflict')),
  CHECK (("state" IN ('matched','conflict')) = ("event_id" IS NOT NULL))
) STRICT;
CREATE INDEX "companion_matches_event" ON "companion_matches" ("event_id");
CREATE TABLE "companion_match_changes" (
  "id" INTEGER NOT NULL,
  "row_id" INTEGER NOT NULL,
  "previous_event_id" INTEGER,
  "event_id" INTEGER,
  "previous_state" TEXT,
  "state" TEXT NOT NULL,
  "reason" TEXT NOT NULL,
  "candidates_json" TEXT NOT NULL,
  "policy" TEXT NOT NULL,
  "changed_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id"),
  FOREIGN KEY ("event_id") REFERENCES "source_events" ("id"),
  FOREIGN KEY ("previous_event_id") REFERENCES "source_events" ("id"),
  FOREIGN KEY ("row_id") REFERENCES "companion_rows" ("id")
) STRICT;
CREATE TRIGGER "companion_match_changes_no_update" BEFORE UPDATE ON "companion_match_changes" BEGIN SELECT RAISE(ABORT, 'companion matching history is append-only'); END;
CREATE TRIGGER "companion_match_changes_no_delete" BEFORE DELETE ON "companion_match_changes" BEGIN SELECT RAISE(ABORT, 'companion matching history is append-only'); END;
CREATE TABLE "category_catalog" (
  "account" TEXT NOT NULL,
  PRIMARY KEY ("account")
) STRICT;
CREATE TABLE "bank_categories" (
  "movement_id" INTEGER NOT NULL,
  "event_id" INTEGER NOT NULL,
  "account" TEXT NOT NULL,
  "reason" TEXT NOT NULL,
  PRIMARY KEY ("movement_id"),
  UNIQUE ("event_id"),
  FOREIGN KEY ("movement_id", "event_id") REFERENCES "operation_movements" ("id", "anchor_event_id")
) STRICT;
CREATE TABLE "bank_category_changes" (
  "id" INTEGER NOT NULL,
  "movement_id" INTEGER NOT NULL,
  "event_id" INTEGER NOT NULL,
  "previous_account" TEXT,
  "account" TEXT NOT NULL,
  "reason" TEXT NOT NULL,
  "changed_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id"),
  FOREIGN KEY ("movement_id", "event_id") REFERENCES "operation_movements" ("id", "anchor_event_id")
) STRICT;
CREATE TRIGGER "bank_category_changes_no_update" BEFORE UPDATE ON "bank_category_changes" BEGIN SELECT RAISE(ABORT, 'category history is append-only'); END;
CREATE TRIGGER "bank_category_changes_no_delete" BEFORE DELETE ON "bank_category_changes" BEGIN SELECT RAISE(ABORT, 'category history is append-only'); END;
CREATE TABLE "day_inventories" (
  "institution" TEXT NOT NULL,
  "source_account" TEXT NOT NULL,
  "booked_on" TEXT NOT NULL,
  "signature" TEXT NOT NULL,
  "document_id" INTEGER NOT NULL,
  PRIMARY KEY ("institution", "source_account", "booked_on"),
  FOREIGN KEY ("document_id") REFERENCES "import_documents" ("id")
) STRICT;
CREATE TRIGGER "day_inventories_no_update" BEFORE UPDATE ON "day_inventories" BEGIN SELECT RAISE(ABORT, 'daily inventories are immutable'); END;
CREATE TRIGGER "day_inventories_no_delete" BEFORE DELETE ON "day_inventories" BEGIN SELECT RAISE(ABORT, 'daily inventories are immutable'); END;
CREATE TABLE "bank_import_attempts" (
  "id" INTEGER NOT NULL,
  "institution" TEXT NOT NULL,
  "path" TEXT NOT NULL,
  "document_id" INTEGER,
  "format" TEXT NOT NULL,
  "outcome" TEXT NOT NULL,
  "inserted_count" INTEGER NOT NULL,
  "existing_count" INTEGER NOT NULL,
  "error" TEXT,
  "attempted_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id"),
  FOREIGN KEY ("document_id") REFERENCES "import_documents" ("id"),
  CHECK ("outcome" IN ('imported','unchanged','rejected','unsupported','unreadable')),
  CHECK ("inserted_count" >= 0),
  CHECK ("existing_count" >= 0),
  CHECK (("outcome" = 'unreadable') = ("document_id" IS NULL))
) STRICT;
CREATE INDEX "bank_import_attempts_document" ON "bank_import_attempts" 
  ("document_id", "id");
CREATE TRIGGER "bank_import_attempts_no_update" BEFORE UPDATE ON "bank_import_attempts" BEGIN SELECT RAISE(ABORT, 'import history is append-only'); END;
CREATE TRIGGER "bank_import_attempts_no_delete" BEFORE DELETE ON "bank_import_attempts" BEGIN SELECT RAISE(ABORT, 'import history is append-only'); END;
CREATE TABLE "document_contexts" (
  "document_id" INTEGER NOT NULL,
  "context_json" TEXT NOT NULL,
  PRIMARY KEY ("document_id"),
  FOREIGN KEY ("document_id") REFERENCES "import_documents" ("id")
) STRICT;
CREATE TRIGGER "document_contexts_no_update" BEFORE UPDATE ON "document_contexts" BEGIN SELECT RAISE(ABORT, 'document contexts are immutable'); END;
CREATE TRIGGER "document_contexts_no_delete" BEFORE DELETE ON "document_contexts" BEGIN SELECT RAISE(ABORT, 'document contexts are immutable'); END;
CREATE TABLE "bank_transfer_decisions" (
  "first_event_id" INTEGER NOT NULL,
  "second_event_id" INTEGER NOT NULL,
  "state" TEXT NOT NULL,
  "reason" TEXT NOT NULL,
  PRIMARY KEY ("first_event_id", "second_event_id"),
  FOREIGN KEY ("second_event_id") REFERENCES "source_events" ("id"),
  FOREIGN KEY ("first_event_id") REFERENCES "source_events" ("id"),
  CHECK ("state" IN ('confirmed','rejected')),
  CHECK ("first_event_id" < "second_event_id")
) STRICT;
CREATE TABLE "bank_transfer_memberships" (
  "event_id" INTEGER NOT NULL,
  "first_event_id" INTEGER NOT NULL,
  "second_event_id" INTEGER NOT NULL,
  PRIMARY KEY ("event_id"),
  FOREIGN KEY ("first_event_id", "second_event_id") REFERENCES "bank_transfer_decisions" ("first_event_id", "second_event_id"),
  FOREIGN KEY ("event_id") REFERENCES "source_events" ("id"),
  CHECK (("event_id" = "first_event_id") OR ("event_id" = "second_event_id"))
) STRICT;
CREATE TABLE "bank_transfer_changes" (
  "id" INTEGER NOT NULL,
  "first_event_id" INTEGER NOT NULL,
  "second_event_id" INTEGER NOT NULL,
  "previous_state" TEXT,
  "state" TEXT NOT NULL,
  "reason" TEXT NOT NULL,
  "changed_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id"),
  FOREIGN KEY ("second_event_id") REFERENCES "source_events" ("id"),
  FOREIGN KEY ("first_event_id") REFERENCES "source_events" ("id"),
  CHECK ("state" IN ('confirmed','rejected'))
) STRICT;
CREATE TRIGGER "bank_transfer_changes_no_update" BEFORE UPDATE ON "bank_transfer_changes" BEGIN SELECT RAISE(ABORT, 'Transfer history is append-only'); END;
CREATE TRIGGER "bank_transfer_changes_no_delete" BEFORE DELETE ON "bank_transfer_changes" BEGIN SELECT RAISE(ABORT, 'Transfer history is append-only'); END;
CREATE TABLE "bank_account_identities" (
  "institution" TEXT NOT NULL,
  "source_account" TEXT NOT NULL,
  "iban" TEXT NOT NULL,
  PRIMARY KEY ("institution", "source_account")
) STRICT;
CREATE TABLE "bank_account_identity_changes" (
  "id" INTEGER NOT NULL,
  "institution" TEXT NOT NULL,
  "source_account" TEXT NOT NULL,
  "previous_iban" TEXT,
  "iban" TEXT,
  "changed_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id")
) STRICT;
CREATE TRIGGER "bank_account_identity_changes_no_update" BEFORE UPDATE ON "bank_account_identity_changes" BEGIN SELECT RAISE(ABORT, 'Account identity history is append-only'); END;
CREATE TRIGGER "bank_account_identity_changes_no_delete" BEFORE DELETE ON "bank_account_identity_changes" BEGIN SELECT RAISE(ABORT, 'Account identity history is append-only'); END;
CREATE TABLE "bank_account_settlements" (
  "institution" TEXT NOT NULL,
  "source_account" TEXT NOT NULL,
  "settlement_account" TEXT NOT NULL,
  PRIMARY KEY ("institution", "source_account")
) STRICT;
CREATE TABLE "bank_account_settlement_changes" (
  "id" INTEGER NOT NULL,
  "institution" TEXT NOT NULL,
  "source_account" TEXT NOT NULL,
  "previous_settlement" TEXT,
  "settlement_account" TEXT,
  "changed_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id")
) STRICT;
CREATE TRIGGER "bank_account_settlement_changes_no_update" BEFORE UPDATE ON "bank_account_settlement_changes" BEGIN SELECT RAISE(ABORT, 'Account settlement history is append-only'); END;
CREATE TRIGGER "bank_account_settlement_changes_no_delete" BEFORE DELETE ON "bank_account_settlement_changes" BEGIN SELECT RAISE(ABORT, 'Account settlement history is append-only'); END;
CREATE TABLE "institutions" (
  "name" TEXT NOT NULL,
  "profile" TEXT,
  "companion_profile" TEXT,
  PRIMARY KEY ("name")
) STRICT;
CREATE TABLE "institution_definitions" (
  "name" TEXT NOT NULL,
  "definition_json" TEXT NOT NULL,
  PRIMARY KEY ("name")
) STRICT;
CREATE TABLE "card_settlement_decisions" (
  "event_id" INTEGER NOT NULL,
  "card_institution" TEXT NOT NULL,
  "card_source_account" TEXT NOT NULL,
  "ledger_account" TEXT NOT NULL,
  "reason" TEXT NOT NULL,
  PRIMARY KEY ("event_id"),
  FOREIGN KEY ("event_id") REFERENCES "source_events" ("id")
) STRICT;
CREATE TABLE "card_settlement_changes" (
  "id" INTEGER NOT NULL,
  "event_id" INTEGER NOT NULL,
  "card_institution" TEXT NOT NULL,
  "card_source_account" TEXT NOT NULL,
  "ledger_account" TEXT NOT NULL,
  "action" TEXT NOT NULL,
  "changed_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id"),
  CHECK ("action" IN ('confirmed','undone'))
) STRICT;
CREATE TRIGGER "card_settlement_changes_no_update" BEFORE UPDATE ON "card_settlement_changes" BEGIN SELECT RAISE(ABORT, 'card settlement history is append-only'); END;
CREATE TRIGGER "card_settlement_changes_no_delete" BEFORE DELETE ON "card_settlement_changes" BEGIN SELECT RAISE(ABORT, 'card settlement history is append-only'); END;
CREATE TABLE "card_assignments" (
  "institution" TEXT NOT NULL,
  "card_reference" TEXT NOT NULL,
  "card_source_account" TEXT NOT NULL,
  PRIMARY KEY ("institution", "card_reference")
) STRICT;
CREATE TABLE "card_assignment_changes" (
  "id" INTEGER NOT NULL,
  "institution" TEXT NOT NULL,
  "card_reference" TEXT NOT NULL,
  "previous_card_source_account" TEXT,
  "card_source_account" TEXT,
  "changed_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id")
) STRICT;
CREATE TRIGGER "card_assignment_changes_no_update" BEFORE UPDATE ON "card_assignment_changes" BEGIN SELECT RAISE(ABORT, 'Card assignment history is append-only'); END;
CREATE TRIGGER "card_assignment_changes_no_delete" BEFORE DELETE ON "card_assignment_changes" BEGIN SELECT RAISE(ABORT, 'Card assignment history is append-only'); END;
CREATE TABLE "classification_rules" (
  "mode" TEXT NOT NULL,
  "account_type" TEXT,
  "priority" INTEGER NOT NULL,
  "name" TEXT NOT NULL,
  "pattern" TEXT,
  "institution" TEXT,
  "match_type" TEXT,
  "case_mode" TEXT NOT NULL,
  "action_kind" TEXT NOT NULL,
  "target" TEXT NOT NULL,
  "action_type" TEXT,
  "field" TEXT,
  PRIMARY KEY ("name"),
  CHECK ("mode" IN ('automatic','review','disabled')),
  CHECK ("case_mode" IN ('sensitive','insensitive')),
  CHECK ("action_kind" IN ('category','settlement','type','type-category','card-reference','withdrawal-reversal'))
) STRICT;
CREATE TABLE "reversal_pairs" (
  "original" INTEGER NOT NULL,
  "reversal" INTEGER NOT NULL,
  "reason" TEXT NOT NULL,
  PRIMARY KEY ("original"),
  FOREIGN KEY ("original") REFERENCES "source_events" ("id"),
  FOREIGN KEY ("reversal") REFERENCES "source_events" ("id")
) STRICT;
CREATE UNIQUE INDEX "reversal_pairs_reversal" ON "reversal_pairs" ("reversal");
CREATE TABLE "reversal_changes" (
  "id" INTEGER NOT NULL,
  "original" INTEGER NOT NULL,
  "reversal" INTEGER NOT NULL,
  "action" TEXT NOT NULL,
  "changed_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id"),
  CHECK ("action" IN ('confirmed','undone'))
) STRICT;
CREATE TRIGGER "reversal_changes_no_update" BEFORE UPDATE ON "reversal_changes" BEGIN SELECT RAISE(ABORT, 'reversal history is append-only'); END;
CREATE TRIGGER "reversal_changes_no_delete" BEFORE DELETE ON "reversal_changes" BEGIN SELECT RAISE(ABORT, 'reversal history is append-only'); END;
CREATE TABLE "movement_type_overrides" (
  "movement_id" INTEGER NOT NULL,
  "event_id" INTEGER NOT NULL,
  "movement_type" TEXT NOT NULL,
  PRIMARY KEY ("movement_id"),
  UNIQUE ("event_id"),
  FOREIGN KEY ("movement_id", "event_id") REFERENCES "operation_movements" ("id", "anchor_event_id")
) STRICT;
CREATE TABLE "movement_type_changes" (
  "id" INTEGER NOT NULL,
  "movement_id" INTEGER NOT NULL,
  "event_id" INTEGER NOT NULL,
  "previous_type" TEXT,
  "movement_type" TEXT,
  "reason" TEXT NOT NULL,
  "changed_at" TEXT NOT NULL DEFAULT ((strftime('%Y-%m-%dT%H:%M:%f','now') || '000000000Z')),
  PRIMARY KEY ("id"),
  FOREIGN KEY ("movement_id", "event_id") REFERENCES "operation_movements" ("id", "anchor_event_id")
) STRICT;
CREATE TRIGGER "movement_type_changes_no_update" BEFORE UPDATE ON "movement_type_changes" BEGIN SELECT RAISE(ABORT, 'movement type history is append-only'); END;
CREATE TRIGGER "movement_type_changes_no_delete" BEFORE DELETE ON "movement_type_changes" BEGIN SELECT RAISE(ABORT, 'movement type history is append-only'); END;
PRAGMA user_version = 40;
