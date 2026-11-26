-- Deploy arkham-horror-backend:add_login_tracking_to_users to pg
-- requires: users
-- requires: arkham_players
-- requires: arkham_decks
-- requires: arkham_achievements
-- requires: arkham_api_keys
-- requires: arkham_build_connections
-- requires: arkham_card_set_shares
-- requires: arkham_card_set_submissions
-- requires: arkham_custom_cards
-- requires: arkham_custom_card_sets
-- requires: arkham_epic
-- requires: arkham_game_archives
-- requires: arkham_published_card_set_likes
-- requires: arkham_third_edition_tables
-- requires: create_password_resets

BEGIN;

-- When the site is invite only, a session may only begin or resume for an
-- account seen within `INVITE_ONLY_DORMANT_AFTER_MONTHS` months (6 by default):
-- a closed server keeps the players it has rather than holding the door for
-- every account that ever existed. Enforcement does not start until
-- `INVITE_ONLY_DORMANT_FROM`, so a week of ordinary traffic corrects the
-- estimate below before anyone is turned away. With invite only off, neither
-- column is read.
--
-- Two columns because they answer different questions. `last_seen_at` is what
-- the cutoff is measured against, and it moves on `GET /api/v1/whoami` as well:
-- a login token here carries no `exp`, so an account in daily use can go years
-- without authenticating again and must not go stale while it is being played.
-- `last_login_at` moves only on `POST /api/v1/authenticate`, which is the one
-- that answers "when did somebody last actually sign in to this account".
ALTER TABLE users
  ADD COLUMN IF NOT EXISTS last_login_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  ADD COLUMN IF NOT EXISTS last_seen_at TIMESTAMPTZ NOT NULL DEFAULT now();

-- Nothing recorded either of these before this migration, so the `now()`
-- default would give every account six free months and the policy would do
-- nothing until mid-2027. Seed instead from any timestamp in the database that
-- is evidence of the account being used, and take the most recent one. Every
-- such row required an authenticated session to create, so the same estimate is
-- the best available answer for both columns.
--
-- An account with no such row keeps `now()`: no evidence of dormancy either,
-- and locking out somebody who registered last week and has not started a game
-- yet is the one outcome worth avoiding here.
--
-- `seen_at <= now()` drops the timestamps that point forward rather than back
-- (an unrevoked key's `expires_at`, a token refresh deadline), and a password
-- reset is credited at its request time -- `expires_at` is set a day ahead.
UPDATE users AS u
SET last_login_at = activity.last_seen
  , last_seen_at = activity.last_seen
FROM (
  SELECT user_id, max(seen_at) AS last_seen
  FROM (
    SELECT p.user_id, g.updated_at AT TIME ZONE 'UTC' FROM arkham_players p
      JOIN arkham_games g ON g.id = p.arkham_game_id
    UNION ALL SELECT user_id, last_used_at FROM arkham_decks
    UNION ALL SELECT user_id, earned_at FROM arkham_achievements
    UNION ALL SELECT user_id, created_at FROM arkham_api_keys
    UNION ALL SELECT user_id, last_used_at FROM arkham_api_keys
    UNION ALL SELECT user_id, usage_window_start FROM arkham_api_keys
    UNION ALL SELECT user_id, revoked_at FROM arkham_api_keys
    UNION ALL SELECT user_id, created_at FROM arkham_build_auth_requests
    UNION ALL SELECT user_id, updated_at FROM arkham_build_connections
    UNION ALL SELECT user_id, taken_at FROM arkham_card_set_share_accesses
    UNION ALL SELECT user_id, updated_at FROM arkham_card_set_share_accesses
    UNION ALL SELECT user_id, updated_at FROM arkham_card_set_submissions
    UNION ALL SELECT reviewed_by_user_id, reviewed_at FROM arkham_card_set_submissions
    UNION ALL SELECT user_id, updated_at FROM arkham_custom_cards
    UNION ALL SELECT user_id, updated_at FROM arkham_custom_card_sets
    UNION ALL SELECT organizer_user_id, updated_at FROM arkham_epic_events
    UNION ALL SELECT requested_by, requested_at FROM arkham_game_archives
    UNION ALL SELECT requested_by, restored_at FROM arkham_game_archives
    UNION ALL SELECT user_id, created_at FROM arkham_published_card_set_likes
    UNION ALL SELECT user_id, updated_at FROM arkham_published_card_sets
    UNION ALL SELECT host_id, updated_at FROM arkham_third_edition_tables
    UNION ALL SELECT user_id, expires_at - INTERVAL '1 day' FROM password_resets
  ) AS signals (user_id, seen_at)
  WHERE user_id IS NOT NULL AND seen_at IS NOT NULL AND seen_at <= now()
  GROUP BY user_id
) AS activity
WHERE activity.user_id = u.id;

COMMIT;
