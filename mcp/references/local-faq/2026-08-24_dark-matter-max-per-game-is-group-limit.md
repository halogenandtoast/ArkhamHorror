---
title: Dark Matter "Max ... per game" is always a group limit
date_added: 2026-08-24
source: internal decision (user instruction) — campaign-specific house ruling for the Dark Matter homebrew campaign only
affects:
  - Dark Matter homebrew campaign
  - groupLimit / playerLimit / limitedAbility
  - Omni-Transmitters
  - Impassable Ravine
  - The Yellow Throne
  - Grand Ballroom
  - Gardens of Thothut
  - Labyrinths of Tasylock
---

# Dark Matter "Max ... per game" is always a group limit

In the Dark Matter homebrew campaign, "Max once per game" (and the whole
"Max &lt;X&gt; per game" family — e.g. "Max one success per game") printed on a
card is **always a GROUP limit**, keyed on the card's title (excluding
subtitle). It is never a per-player limit, even though the Arkham Grimoire's
general-purpose default for unscoped "once per game" text is per-player.

This ruling **overrides** the Grimoire default for this campaign. An earlier
implementation pass had applied the Grimoire per-player default to several
Dark Matter cards; those have been reverted to group-wide.

In engine terms: use `groupLimit PerGame` (or an equivalent single
game-wide gate, e.g. a semaphore modifier attached to the ability's own
source/entity rather than to the triggering investigator), never
`playerLimit PerGame`, for any Dark Matter card whose printed text reads
"Max ... per game."

## Affected cards / systems

- Omni-Transmitters — `backend/arkham-api/library/Arkham/Homebrew/DarkMatter/Locations/OmniTransmitters.hs` — "(Max one success per game.)"
- Impassable Ravine — `backend/arkham-api/library/Arkham/Homebrew/DarkMatter/Locations/ImpassableRavine.hs` — "(Max once per game.)" ("Lost Expedition")
- The Yellow Throne — `backend/arkham-api/library/Arkham/Homebrew/DarkMatter/Locations/TheYellowThrone.hs` — "(Max once per game.)" ("Lost Expedition")
- Grand Ballroom — `backend/arkham-api/library/Arkham/Homebrew/DarkMatter/Locations/GrandBallroom.hs` — "(Max once per game.)" ("Arrival of the King")
- Gardens of Thothut — `backend/arkham-api/library/Arkham/Homebrew/DarkMatter/Locations/GardensOfThothut.hs` — "(Max once per game.)" ("Delights")
- Labyrinths of Tasylock — `backend/arkham-api/library/Arkham/Homebrew/DarkMatter/Locations/LabyrinthsOfTasylock.hs` — "(Max once per game.)" ("For You Alone")

Already-correct, unaffected by this fix (already used `groupLimit PerGame` or
an equivalent game-wide mechanism): Classroom K-2, Telecoms, Crystal Peak,
Hydroponics, Main Facility, Crew Quarters, City of Cats, A Mutiny, Stasis
Cube, Adam Tanner, Sophie, Lt. "Archer" Michaels, Doctor Feng, MUD-12
"Mudbug", and the Out of Mind agenda's built-in agenda-meta flag.

Note on keying: the engine's `GroupLimit` accounting sums ability usage by
ability equality (source card code + ability index), not by card title. As of
this ruling's audit, no two Dark Matter cards that both print a "Max ... per
game" ability share a title under different card codes/subtitles, so the
existing code-keyed `groupLimit` is behaviorally equivalent to a
title-keyed limit. If such a pair is ever added (e.g. an alternate printing
of a Max-per-game card), the limit will need to be shared explicitly — flag
this to the implementing agent at that time.

## Implementation status

- **Omni-Transmitters**: ✏️ updated — reverted the per-investigator-keyed
  success semaphore (`semaphore iid`) back to a single game-wide semaphore
  keyed on the location's own attrs (`semaphore attrs`), keeping the
  "consumed only on success" accounting intact.
- **Impassable Ravine**: ✏️ updated — `playerLimit PerGame` → `groupLimit PerGame`.
- **The Yellow Throne**: ✏️ updated — `playerLimit PerGame` → `groupLimit PerGame`.
- **Grand Ballroom**: ✏️ updated — `playerLimit PerGame` → `groupLimit PerGame`.
- **Gardens of Thothut**: ✏️ updated — `playerLimit PerGame` → `groupLimit PerGame`.
- **Labyrinths of Tasylock**: ✏️ updated — `playerLimit PerGame` → `groupLimit PerGame`.
- All six edits compiled cleanly (`.claude/build.log`, `--pedantic`) and pass
  `hlint`/`fourmolu` on the touched files.
- **Everything else listed above**: ✅ already matched the ruling — no change.
