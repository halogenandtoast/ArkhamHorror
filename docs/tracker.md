# The tracker

Bugs and feature requests are filed in the app and worked on a board in the app.
Nothing is filed on GitHub, and nothing syncs to it.

## Why it moved

Filing used to open `github.com/halogenandtoast/ArkhamHorror/issues/new` with
the game's export URL pasted into the body, and triage happened on GitHub
project #2 ("Arkham Issues"). That cost:

- a reporter needed a GitHub account to say anything at all;
- a reply reached them only if they went back to a site they had no other
  reason to visit;
- the board was a second copy of the truth, so a status was only as good as the
  last sweep over it.

## The shape of it

Three tables (`migrations/deploy/create_reports.sql`):

| table | what it holds |
| --- | --- |
| `reports` | the report: kind, title, body, board column, who filed it, the game it came from |
| `report_events` | the communication log — replies and column moves in one stream |
| `report_views` | when each follower last looked, which is all "unread" means |

A report's `number` is its public name and it **continues the GitHub issue
numbering**: issue #6068 imported is report #6068, and the next thing filed takes
the number above the highest imported one. Every `#number` already written into
a commit message still points at the same thing.

`kind` is `bug` or `feature`. `status` is a board column:

```
new → todo → in-progress → blocked → ready-for-deploy → pending-deploy
                                        done | declined | duplicate
```

The first six mirror project #2's Status field, including the untriaged column
that board expressed as having no status. `kind` carries a CHECK constraint and
`status` does not: a third kind would be a change to what the tracker is, while
another column is a day's decision, and a CHECK would make it a migration.

Bugs and feature requests get a **board each**, never one board with both on it:
they are different work and the same column means a different thing on each. The
split is in the client -- the kind is in the URL, the server sends the whole
board in one read -- so `position` orders a column within the board it belongs
to and the two never interleave on screen.

Cards drag between columns and within one. A drop sends the destination column's
whole new order and the server rewrites its positions from 1, which is what
keeps ordering out of fractional arithmetic and makes the same drop twice idem-
potent. Every card also carries a menu with the same moves, because drag works
on neither a phone nor a keyboard.

Six open columns do not fit in a narrow window, so the board scrolls sideways
and says so: columns are sized against the board's own width rather than the
window's, never wide enough to fill it, and the end with more board behind it
carries a fade and an arrow. macOS hides its scrollbars until something is
already moving, which is exactly when nobody needs telling.

### Pages

| page | who |
| --- | --- |
| `/reports` | everyone signed in — searchable, defaults to open work |
| `/reports/new` | filing from outside a game |
| `/reports/:number` | the report, its game, and the conversation |
| `/admin/board/:kind` | the boards, admin-only: one for bugs, one for features |

The in-game **Bug** button files straight into the tracker and the server takes
the export itself, so there is nothing to paste. The whole thing happens in the
one panel over the game -- the form, the wait, the failure, the report number --
because the only reason the old flow took the game away was that it was handing
the player to GitHub. Third edition files here too
(`frontend-3ed/src/game/BugTools.vue`); a 3e table is not an Arkham game, so its
report carries the table and its export as text rather than a `game_id`.

### The game a report came from

`game_id` points at the game, and `game_name` and `game_step` are **copied onto
the report** at filing. The copy is the point: the game may be deleted, and a
report that then cannot say what it was about is useless. The report page shows
the step it was filed at against the game's step now — a reporter who played on
for forty steps has given the fix a moving target.

`export_url` is only sent to an admin. The tracker is public and an export is
the whole game state.

## Migrating the open issues

One-off, idempotent by issue number, re-runnable:

```bash
./scripts/export-github-issues.py --out /tmp/issues.json
```

Then either drop that file into the **Import from GitHub** box on
`/admin/board`, or:

```bash
curl -X POST https://arkhamhorror.app/api/v1/admin/reports/import \
  -H 'Authorization: Token <admin jwt>' \
  -H 'Content-Type: application/json' \
  --data @/tmp/issues.json
```

The script is separate from the server on purpose: the GitHub token stays on the
machine running it, the import is an ordinary admin write, and nothing in the
app has to hold a credential for a migration that happens once.

What comes across: the number, title, body, board column (or `new`), close
reason (`done` / `declined` / `duplicate`), the `enhancement` label as
`kind = feature`, every comment, and the game id out of the body's `game:` line
when that game still exists. What cannot: authorship — a GitHub login is not an
account here, so the reporter is named in the text, `author_id` is left NULL,
and an imported report has nobody to send unread badges to.

The issues themselves are left alone. Nothing here writes to GitHub.

## Triaging from the terminal

`/herd-issues` fanned the board's Todo column out into one Claude agent per
issue, each running `/investigate-issue`. The tracker's twins are
`/herd-reports` and `/investigate-report`, and they do the same thing from the
same three pieces:

| | GitHub | tracker |
| --- | --- | --- |
| list a column | `.claude/scripts/board-issues.sh Todo` | `.claude/scripts/arkham-reports.sh list todo` |
| spawn the agents | `.claude/scripts/herd-issues.sh` | `.claude/scripts/herd-reports.sh` |
| investigate one | `/investigate-issue <url>` | `/investigate-report <N>` |

`arkham-reports.sh` is the whole of what replaces `gh` here: `list`, `show`,
`export`, plus `login` and `whoami`. It points at `$ARKHAM_API` (default
`http://127.0.0.1:3002`) and reads a token from `$ARKHAM_TOKEN` or
`~/.config/arkham/token`.

Two things the GitHub path did not have to say:

* **Every read needs a token.** The tracker is readable by anyone with an
  account, not by anyone at all, so even listing a column is a 401 signed out.
  There is no anonymous `gh issue view` equivalent and there should not be one.
* **The token has to be an admin**, because `game.exportUrl` is only sent to an
  admin and the export is the thing `arkham-replay` eats. `whoami` says which
  you have; the commands check it before spawning anything.

Against that, two things got easier. The export is a field on the report rather
than a URL to find in the prose after the word `file:`, and the replies come
back in the same call as the body, so an agent reads the whole conversation
without a second request.

Nothing in either command writes to the tracker. Replying and moving a report
between columns stay manual, on the report page or the board.

## Still pointing at GitHub

`.claude/commands/investigate-issue.md`, `herd-issues.md` and
`.claude/scripts/board-issues.sh` still read issues with `gh`. They are left in
place until the tracker is deployed and the import has run — until then GitHub
is still where the open work is. Once it is not, all three can go.

`POST /api/v1/arkham/games/#ArkhamGameId/file-bug` uploads an export and returns
its URL. Nothing calls it any more: the report endpoint does that itself. Its 3e
twin is still used, for the export the 3e report quotes.
