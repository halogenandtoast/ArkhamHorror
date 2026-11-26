-- Deploy arkham-horror-backend:create_reports to pg
-- requires: users
-- requires: arkham_games

BEGIN;

-- The tracker. Bugs and feature requests were filed as GitHub issues and worked
-- on a GitHub project board, which meant a reporter needed a GitHub account to
-- say anything and could only be answered somewhere they had no reason to go
-- back to. Both halves move here: filing, and the board.
--
-- `number` continues the issue numbering rather than starting again, so every
-- `#61` already written into a commit message still points at the same report
-- once the open issues are imported. New reports take the next number above the
-- highest imported one.
--
-- `kind` has a CHECK and `status` does not, which is deliberate: a report is a
-- bug or a request for something, and a third kind would be a change to what the
-- tracker is. A board column is not -- adding one is a day's decision, and a
-- CHECK would make it a migration. Both sets are enforced at the handler, which
-- is where a bad value can still be answered with a 400 instead of a row that
-- will not decode.
CREATE TABLE IF NOT EXISTS reports (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  number int NOT NULL,
  kind text NOT NULL,
  title text NOT NULL,
  body text NOT NULL,
  -- 'new' (filed, untriaged) / 'todo' / 'in-progress' / 'blocked' /
  -- 'ready-for-deploy' / 'pending-deploy' / 'done' / 'declined' / 'duplicate'.
  status text NOT NULL DEFAULT 'new',
  -- Nulled rather than cascaded: a tracker that forgets a report because its
  -- reporter deleted their account loses the bug, not just the name.
  author_id bigint REFERENCES users (id) ON DELETE SET NULL,
  -- Who filed it when that is not an account here: the GitHub login on an
  -- imported issue. Read only when `author_id` is NULL, and never a claim that
  -- the name belongs to anybody on this site.
  author_name text,
  -- The reporter says their game cannot go on. Their claim, not a priority.
  blocking boolean NOT NULL DEFAULT false,
  duplicate_of_id uuid REFERENCES reports (id) ON DELETE SET NULL,
  -- Off the public list without being deleted, for whatever a public tracker
  -- turns out to attract. Still reachable by its author and by an admin.
  hidden boolean NOT NULL DEFAULT false,
  -- The game it was filed from, while that game lasts.
  game_id uuid REFERENCES arkham_games (id) ON DELETE SET NULL,
  -- What the game was called and what step it was on at filing. Kept beside the
  -- reference so the report still says what it was about after the game is gone,
  -- and so the step it was reported at can be read against the game's step now.
  game_name text,
  game_step int,
  export_url text,
  -- Order within the board column. Rewritten 1..n across a whole column when
  -- anything in it moves, so there is no fractional arithmetic to exhaust.
  position double precision NOT NULL DEFAULT 0,
  created_at timestamptz NOT NULL DEFAULT now(),
  -- Touched by anything that happens to the report. A follower's `viewed_at` is
  -- compared against this, and that comparison is the whole of "unread".
  updated_at timestamptz NOT NULL DEFAULT now(),
  CONSTRAINT unique_report_number UNIQUE (number),
  CONSTRAINT valid_report_kind CHECK (kind IN ('bug', 'feature'))
);

-- The board reads a column at a time; the list filters by status.
CREATE INDEX IF NOT EXISTS idx_reports_status ON reports (status);
-- "My reports", for the badge and the home-page panel.
CREATE INDEX IF NOT EXISTS idx_reports_author ON reports (author_id);
-- Whether a game already has a report filed against it.
CREATE INDEX IF NOT EXISTS idx_reports_game ON reports (game_id);

-- The communication log: replies and column moves in one stream, so a report
-- reads as a conversation rather than a record with a changelog beside it.
--
-- `kind` says which of the two payload columns to read -- a 'comment' carries
-- `body`, a 'status' carries the column it moved to. One table rather than two
-- because the page wants them interleaved in time, which is a sort, not a merge.
CREATE TABLE IF NOT EXISTS report_events (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  report_id uuid NOT NULL REFERENCES reports (id) ON DELETE CASCADE,
  -- Null once the account that wrote it is gone, or for anything the server did
  -- on nobody's behalf.
  user_id bigint REFERENCES users (id) ON DELETE SET NULL,
  -- As on `reports`: the name to show when the writer is not an account here.
  author_name text,
  kind text NOT NULL,
  body text,
  status text,
  created_at timestamptz NOT NULL DEFAULT now(),
  CONSTRAINT valid_report_event_kind CHECK (kind IN ('comment', 'status'))
);

CREATE INDEX IF NOT EXISTS idx_report_events_report
  ON report_events (report_id, created_at);

-- When each follower last looked at a report, which is all "unread" means. A row
-- per viewer rather than one timestamp on the report: a public tracker has no
-- single reader for the report to be behind.
CREATE TABLE IF NOT EXISTS report_views (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  report_id uuid NOT NULL REFERENCES reports (id) ON DELETE CASCADE,
  user_id bigint NOT NULL REFERENCES users (id) ON DELETE CASCADE,
  viewed_at timestamptz NOT NULL DEFAULT now(),
  CONSTRAINT unique_report_view UNIQUE (report_id, user_id)
);

CREATE INDEX IF NOT EXISTS idx_report_views_user ON report_views (user_id);

COMMIT;
