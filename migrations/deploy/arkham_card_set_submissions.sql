-- Deploy arkham-horror-backend:arkham_card_set_submissions to pg
-- requires: arkham_published_card_sets

BEGIN;

-- Review for the marketplace. Publishing used to be the whole of getting a set
-- listed; now it is a submission, and a version is importable only once someone
-- has approved it.
--
-- The submission is its own table rather than columns on the version because a
-- version is what was published and must not change, while a review is a second
-- thing that happens to it later -- and because a denial has a reason, a
-- reviewer and a time that have nowhere to live on a snapshot.
--
-- One row per version: a version is reviewed once. Re-submitting is publishing
-- again, which makes a new version and so a new submission.
--
-- `notify` is the author's answer to "tell me what was decided", asked at the
-- time they submit rather than kept as an account setting: it is a choice about
-- this submission, and someone who wants to hear about one may not want to hear
-- about the next.
--
-- `reason` is only ever set on a denial. An approval that needs explaining is a
-- denial.

CREATE TABLE IF NOT EXISTS arkham_card_set_submissions (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  published_card_set_id uuid NOT NULL
    REFERENCES arkham_published_card_sets (id) ON DELETE CASCADE,
  version_id uuid NOT NULL
    REFERENCES arkham_published_card_set_versions (id) ON DELETE CASCADE,
  user_id bigint NOT NULL REFERENCES users (id) ON DELETE CASCADE,
  version int NOT NULL,
  status varchar NOT NULL DEFAULT 'pending',
  notify boolean NOT NULL DEFAULT TRUE,
  reason varchar,
  reviewed_by_user_id bigint REFERENCES users (id) ON DELETE SET NULL,
  reviewed_at timestamptz,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  CONSTRAINT unique_card_set_submission_version UNIQUE (version_id)
);

-- The review queue is "every pending submission, oldest first", which is the
-- one query this table exists to answer.
CREATE INDEX IF NOT EXISTS idx_arkham_card_set_submissions_status
  ON arkham_card_set_submissions (status, created_at);

CREATE INDEX IF NOT EXISTS idx_arkham_card_set_submissions_set
  ON arkham_card_set_submissions (published_card_set_id);

-- The newest version anyone has approved, and so the one the marketplace shows
-- and hands out. Kept on the listing rather than derived from the submissions
-- every time, because every listing read needs it and a listing read is the
-- marketplace's front page.
--
-- Null means nothing of this set is approved yet, which is what hides it.
ALTER TABLE arkham_published_card_sets
  ADD COLUMN IF NOT EXISTS approved_version int;

-- Everything already listed was listed before there was a review to pass, so it
-- keeps its place: its newest version is approved, and so are the older ones,
-- which someone may already be subscribed to.
UPDATE arkham_published_card_sets
   SET approved_version = latest_version
 WHERE approved_version IS NULL;

INSERT INTO arkham_card_set_submissions
  (published_card_set_id, version_id, user_id, version, status, notify,
   reviewed_at, created_at, updated_at)
SELECT s.id, v.id, s.user_id, v.version, 'approved', FALSE,
       v.created_at, v.created_at, v.created_at
  FROM arkham_published_card_set_versions v
  JOIN arkham_published_card_sets s ON s.id = v.published_card_set_id
 ON CONFLICT (version_id) DO NOTHING;

COMMIT;
