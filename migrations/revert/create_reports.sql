-- Revert arkham-horror-backend:create_reports from pg

BEGIN;

-- Dropped in dependency order: events and views both point at reports.
DROP TABLE IF EXISTS report_views;
DROP TABLE IF EXISTS report_events;
DROP TABLE IF EXISTS reports;

COMMIT;
