# Archiving old games' undo history

`arkham_steps` is the database. It holds 94 of 98 GB, around 620 rows per game,
and nothing has ever pruned it: the only `DELETE` in the application removes
steps *above* the one a game is sitting on, so a campaign finished two years ago
still carries every patch it ever generated, against the possibility of an undo
nobody is going to ask for.

Archiving moves that history out. A game nobody has touched in six months has
its steps written to S3 as one gzipped debug export and the rows deleted. The
game stays playable — only undo is affected — and the first player to press Undo
is offered the history back.

## Pieces

| Where | What |
|---|---|
| `backend/arkham-api/library/Api/Arkham/Archive.hs` | All of the machinery: the sweep, one game's archive and restore, and the workers. |
| `backend/arkham-api/library/Api/Handler/Arkham/Archive.hs` | `GET /games/#/archive`, `PUT /games/#/unarchive`, and `requireUnarchived`, which every undo endpoint calls. |
| `backend/arkham-api/library/Api/Handler/Arkham/Admin/Archives.hs` | `GET /admin/archives`, `POST /admin/archives/run`. |
| `backend/arkham-api/library/Entity/Arkham/GameArchive.hs` | One row per game: where its history is and what state it is in. |
| `migrations/deploy/arkham_game_archives.sql` | The table, and the trigger exemption that lets the archiver delete steps. |
| `frontend/src/components/admin/Archives.vue` | The panel on `/admin`. |

## Configuration

Two separate switches, because they are two separate decisions.

`ARCHIVE_BUCKET` is the feature. Unset — the default, and so the case in
development — nothing is uploaded, nothing can be archived or restored, and the
endpoints 404. There is deliberately no default bucket name: a deployment that
has not been told where to put the data has no business deleting it.

`ARCHIVE_SWEEP` is only the *nightly job*. **It is off by default**, which means
a bucket on its own gets you archiving and restoring that happen when somebody
asks for them and never otherwise. That is the order to do this in: archive a
game you picked, restore it, look at it, and only then let a job choose games by
age.

| Env | Default | What |
|---|---|---|
| `ARCHIVE_BUCKET` | *(unset — feature off)* | S3 bucket for the exports. |
| `ARCHIVE_SWEEP` | `false` | Whether the nightly job runs. Off is not the feature off: archiving and restoring still work, by hand. |
| `ARCHIVE_AFTER_MONTHS` | `6` | How long a game must be untouched to be a *sweep* candidate. A hand-picked game is not subject to it. |
| `ARCHIVE_BATCH_SIZE` | `10` | Games per batch. |
| `ARCHIVE_BATCH_PAUSE_SECONDS` | `20` | Pause between batches. With the batch size, the whole throttle: ~1,800 games an hour. |

### Setting it up in production

**The migration goes first, before the image.** `requireUnarchived` runs on every
undo and queries `arkham_game_archives` whether or not a bucket is configured, so
new code against an old schema is a failed undo for everyone. The migration
itself is additive and the old code ignores it, so there is no window where the
order hurts.

1. Apply the `arkham_game_archives` migration.
2. Create the bucket. **In `us-east-1`**, like `arkham-horror-bugs`: nothing sets
   `AWS_REGION`, so amazonka defaults there. Private — these are players' games.
   (A bucket in another region gives a 301 on upload. That fails safe — nothing
   is deleted — but it archives nothing.)
3. The existing `AWS_ACCESS_KEY_ID` needs `s3:PutObject`, `s3:GetObject`,
   `s3:DeleteObject` and `s3:ListBucket` on it (HEAD needs `GetObject`).
4. Add to the `app_secrets` map in `terraform/terraform.tfvars`:

   ```hcl
   ARCHIVE_BUCKET = "arkham-horror-archives"
   ```

   No terraform code change — the map's keys become env var names. But `env_from`
   is resolved at **container start**, so it needs `terraform apply` *and*
   `kubectl -n arkham rollout restart deployment/arkham-web`.

Nothing happens on its own after that, because `ARCHIVE_SWEEP` is off. Then, on
`/admin`:

5. Paste a game id into **Archive one game** and press it. This ignores the
   retention cutoff — it refuses a game played in the last day unless you ask
   twice, which is a guard against a mistyped id, not against the operation. The
   response says what was stored, or exactly why it could not be.
6. Press **Restore** on that row (or press Undo in the game, which is what a
   player would hit). Confirm the history comes back and the game plays.

That is the whole mechanism exercised on one game you chose, with the object
intact either way. Only then set `ARCHIVE_SWEEP=true` and deploy again — the
restart is the kick-off, and a minute later one replica claims a run and starts
draining the backlog.

## How a sweep runs

A thread per replica tries to claim a run a minute after boot and then hourly;
the claim is one `INSERT ... ON CONFLICT ... WHERE refreshed_at < now() - 20
hours` against the `arkham_stats_refreshes` row named `game_archive_sweep`, so
the row that records when the sweep last ran *is* the lock, and only one replica
proceeds. A rolling restart of six replicas therefore produces one run, not six.

Then: candidates oldest-first, in batches, pausing between them, **until the
backlog is empty**. At the default 10 games every 20 seconds that is about
eleven hours for twenty thousand games — one night.

Three other ways a run ends. A whole batch failing, because the realistic cause
is the bucket being wrong and the useful response is one error rather than
twenty thousand. Fifty individual failures. Or maintenance mode coming on: a
run is thousands of deletes against the table whose lock someone has just taken
the site down to get, and the middleware cannot see a background job.

A game that fails is excluded from the rest of that run. That is not politeness
— without it the sweep cannot terminate, because a failure writes no archive row,
so the game stays the *oldest* candidate and the next batch picks it up again
forever.

`POST /admin/archives/run` sweeps the whole backlog on demand — it works even
with `ARCHIVE_SWEEP` off, since it is an explicit request (`?force=true`
overrides the one-hour guard). `POST /admin/archives/#ArkhamGameId` archives one
named game, ignoring the cutoff; that is the one to use first. Both have buttons
in the panel.

For each game: upload, **HEAD the object, then download it again and check its
SHA-256 against the file that was written and that `stepLines` can read it and
finds the right number of steps** — and only then delete the rows and write the
archive row, in one transaction.

That read-back is the whole safety argument. A HEAD proves something landed; the
hash proves it is the same bytes, and running the restore's own reader over it
proves the reader and the writer have not drifted apart. It costs one extra
download per game, against finding out months later that an archive cannot be
read. A failure anywhere before the delete leaves an orphaned object, which
costs pennies and is deleted on the spot. The other order loses a player's
history.

Nothing in the archive path decodes game data. Steps are read and written as raw
`jsonb` and the game blob comes from `ArkhamGameRaw`, so no `Game` and no
`Message` is ever parsed. This is not an optimisation: the games most worth
archiving are the oldest, and an engine that has moved on for six months may not
be able to decode them any more. A sweep built on the typed entities would fail
on exactly the rows it exists to collect — and a restore would be unable to put
back what it had faithfully stored.

The object is the same shape the full-export endpoint serves, gzipped, so an
archive can be downloaded and read, or fed straight to `POST /games/import`.

## How a restore runs

**An archived game is not a game that cannot undo.** Archiving does not stop a
game being played, so one can sit above its own seam: steps `0..top_step` are in
the bucket and everything done since is in the database as usual. An undo that
only needs those later steps is an ordinary undo and is not interrupted. The
question is never "is this game archived" but "does *this* undo reach below the
seam".

So the check is not a gate at the top of the handler — it is `checkArchiveSeam`,
called where the target step is known: in `stepBack` (which reads down to
`n - 1`) and in `stepBackToRawStep` (which reads everything above `toStep`, so
down to `toStep + 1`). Both refuse with `409` and the archive attached, which is
the client's cue to offer the fetch.

That placement is a correctness requirement, not a nicety. A multi-step undo
past the seam would find only the rows that survived archiving, apply that
partial set of inverse patches, and leave the game in a state that never
existed.

Nothing warns anyone in advance: carrying an archived flag in the game payload
would put a field on every socket update that is false for all but a handful of
games.

`PUT /games/#/unarchive` moves the row `archived -> restoring` with a conditional
update (so two players pressing Undo start one download, not two), returns
immediately, and forks the work. Steps go back in through `jsonb_to_recordset`
with `ON CONFLICT DO NOTHING`, so a game that was played *after* being archived
keeps the steps above the archive, and a retry is harmless. The outcome reaches
the table as `GameUnarchived` or `GameUnarchiveFailed` over the game's websocket
— which is also how it reaches a player whose socket is on a different replica
than the request was.

A game is only a candidate again once it is fully `restored`. Not merely "not
archived": a game whose restore *failed* has its old steps only in the object
and its newer ones in the database, so archiving it would write a second object
holding the tail of the history and then delete the one holding its beginning.

A restore that dies with its replica is picked up by `restoreWorker` after ten
minutes. A restore that *fails* goes to `failed` rather than back to `archived`:
the object is still there, but something about it did not work, and retrying on a
loop would just fail on a loop. Pressing the button again retries it.

## The trigger exemption

Two triggers on `arkham_steps` exist to keep a game's step chain intact, and one
of them exists precisely to stop anything deleting the step a game is sitting on.
Archiving has to. Rather than `ALTER TABLE ... DISABLE TRIGGER` — which is DDL, takes
an `ACCESS EXCLUSIVE` lock on the busiest table in the application, and lifts the
guard for every other session while it is off — the guards learn one exemption:

```sql
IF coalesce(current_setting('arkham.archiving', true), '') = 'on' THEN RETURN OLD;
```

The archiver sets it with `set_config(..., true)`, which is transaction-local, so
the exemption cannot outlive the statement that needed it and no other session
can see it.

`POST /games/import` still disables the insert trigger the old way. That is
pre-existing and is a separate thing to clean up.

## What the panel says

`/admin` carries games archived, step rows offloaded, bytes in the bucket, and
the backlog (capped — it is a predicate over the whole games table, and the page
only needs to know whether it is large). Plus when the last sweep ran and what it
cost, restores in flight, and the last 25 archives with their state.

One live object per row: a re-archive deletes the one it replaces, and a restore
leaves the object where it is, so "bytes in the bucket" is the sum of the rows.

## Known gaps

- `GET /games/#/replay/#` reads steps directly and is not gated. On an archived
  game it reports `totalSteps: 0` and shows the game as it stands, rather than
  saying the history is in cold storage.
- Nothing shows a game as archived in the games list. The state is only surfaced
  when an undo is refused.
- Deleting a game cascades its archive row away but leaves the object in the
  bucket. Harmless, and cheap, but it is a slow leak: a periodic `ListObjects`
  against the rows would be the way to collect it.
- The S3 round trip cannot be exercised locally — `newEnv discover` has no
  endpoint override, so there is no pointing it at a local MinIO. What *is*
  verified locally is that a failed upload deletes nothing: with a bucket that
  cannot exist, the sweep runs, archives zero games, aborts after the first
  all-failed batch, and leaves every step row in place.
