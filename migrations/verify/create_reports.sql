-- Verify arkham-horror-backend:create_reports on pg

BEGIN;

-- Each column the handlers read, named explicitly: a table that exists with a
-- column missing passes a bare `SELECT 1`.
SELECT id, number, kind, title, body, status, author_id, author_name, blocking, duplicate_of_id,
       hidden, game_id, game_name, game_step, export_url, position, created_at, updated_at
FROM reports WHERE false;

SELECT id, report_id, user_id, author_name, kind, body, status, created_at
FROM report_events WHERE false;

SELECT id, report_id, user_id, viewed_at
FROM report_views WHERE false;

ROLLBACK;
