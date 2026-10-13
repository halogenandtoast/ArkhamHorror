-- Verify arkham-horror-backend:add_features_to_users on pg

BEGIN;

SELECT features FROM users WHERE false;

ROLLBACK;
