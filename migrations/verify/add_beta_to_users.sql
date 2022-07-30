-- Verify arkham-horror-backend:add_beta_to_users on pg

BEGIN;

SELECT beta FROM users WHERE false;

ROLLBACK;
