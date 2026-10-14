-- Deploy arkham-horror-backend:arkham_third_edition_tables to pg
-- requires: users

BEGIN;

-- Third-edition tables, which until now lived only in Redis.
--
-- Redis was the wrong home for them. A table is a game in progress, not a
-- cache entry: the managed cluster runs `allkeys_lru`, so a live game is as
-- evictable as anything else under memory pressure; the key expires two weeks
-- after the last move; and a table that cannot be decoded after a deploy is
-- gone with no trace. Postgres is where the first edition's games already are.
--
-- The whole table -- seats, options, and the engine's `Game` -- rides in
-- `payload`. The engine owns that shape and changes it often, so giving each
-- field a column would mean a migration per engine change, for data no SQL
-- query needs to read. The three things the lobby does ask about -- who hosts
-- it, whether it has started, and which version a client is holding -- are
-- lifted out beside it, and seats are reached through a jsonb index.
CREATE TABLE arkham_third_edition_tables (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    host_id bigint NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    started boolean NOT NULL,
    version int NOT NULL,
    payload jsonb NOT NULL,
    created_at timestamptz NOT NULL,
    updated_at timestamptz NOT NULL
);

-- The lobby's two lists: everything still open to join, and the tables a given
-- user sits at (`payload -> 'seats' @> '[{"user": 1}]'`).
CREATE INDEX arkham_third_edition_tables_open_idx
    ON arkham_third_edition_tables (created_at DESC)
    WHERE NOT started;
CREATE INDEX arkham_third_edition_tables_seats_idx
    ON arkham_third_edition_tables USING gin ((payload -> 'seats') jsonb_path_ops);

-- Undo history: one row per earlier game state, newest by id, capped by the
-- application. One row per state rather than an array on the table, so a move
-- writes one state instead of rewriting all hundred of them.
CREATE TABLE arkham_third_edition_history (
    id bigserial PRIMARY KEY,
    table_id uuid NOT NULL REFERENCES arkham_third_edition_tables (id) ON DELETE CASCADE,
    game jsonb NOT NULL,
    created_at timestamptz NOT NULL
);

CREATE INDEX arkham_third_edition_history_table_idx
    ON arkham_third_edition_history (table_id, id DESC);

COMMIT;
