-- Verify arkham-horror-backend:add_payload_to_log_entries on pg

BEGIN;

SELECT payload, seq FROM arkham_log_entries WHERE false;

ROLLBACK;
