---
name: project_dostep_countdown_must_not_overshoot_reserved_steps
description: "A DoStep countdown that reserves a low step for a later phase both overshoots and skips it; put the entering-play tokens on at creation instead so the counter means only \"remaining\" (#6003)"
metadata:
  node_type: memory
  type: project
  originSessionId: ffd9a798-5aac-4190-9756-f01bfd3b4c6e
  modified: 2026-11-16T13:22:20.055Z
---

Don't reserve low `DoStep` numbers for a *following* phase of the same setup. The
counter then means two things at once — "things still to place" and "which phase" —
and a wide tier makes it overshoot the reserved step instead of landing on it.

Defuse the Explosives (85024, The Blob That Ate Everything) started at
`doStep (1 + playerCount)`, with step 1 = "put 5 damage on The Military's Plan" and
step 0 = "set `meta` True, arming the two Forced abilities". So at step `n` the tokens
owed were `n - 1`, but the bulk-place guard read `length locations <= n`. With a tier of
exactly `n` candidates it placed horror on all `n` (one over the printed
`1 per_investigator`) and `doStep (n - n)` jumped to `DoStep 0`, **skipping step 1**. The
plan entered play with `tokens: []`, which is itself the trigger for the story's second
Forced ability ("If The Military's Plan has no damage on it"), so the explosives
detonated during setup: 3 direct damage to everyone at an explosives location and
Subject 8L-08 healed 5. Reported game: 5 horror for 4 investigators, 0 damage (#6003).

**Tightening the comparison to `< n` is the wrong fix** — it treats the symptom and was
the first thing tried. The card reads "Put this card and the set-aside The Military's
Plan asset into play next to the act deck, **with 5 damage tokens on it**. Place 1 horror
token on …", so the damage belongs with the asset entering play, *before* the horror
placement. Deferred to a trailing step it also meant the plan sat visibly at 0 damage
through all `playerCount` `leadChooseOneM` prompts, which is how the owner spotted it.

The fix places the tokens at creation and deletes the reserved step, so the counter
means only "remaining" and the terminal is 0 — at which point the guard is correctly
`<=` again:

```haskell
plan <- getSetAsideCard Assets.theMilitarysPlan
obtainCard plan                         -- createAssetAt_ does this; createAssetAt does NOT
planId <- createAssetAt plan Global     -- returns the AssetId, pushing CreateAssetAt
placeTokens attrs planId #damage 5      -- flushed after the create, so the asset exists
doStep =<< getPlayerCount
```

`createAssetAt` + `placeTokens` is the idiom for "enters play with tokens on it" —
prior art at `Arkham/Scenario/Scenarios/TheMidwinterGala.hs:485-487` (Jewel of Sarnath).
Simulated with the tier shrinking the way `not_ (LocationWithToken #horror)` makes it
shrink, over every distance map for 1-4 players: old 393 wrong, new 0.

Two companions:
- The branch had a **latent infinite loop**: empty `locations` with the counter still
  positive means `doStep (n - 0)` re-enters the identical step forever. Guard `null
  locations` by jumping to the terminal step. This survives the restructure.
- **Renumbering steps breaks games parked mid-setup.** A live queue holds `DoStep k`
  from the old scheme, so after the rebuild `k` is read under the new meaning — here a
  pending `DoStep 1` went from "place the damage" to "place one more horror token", then
  armed the abilities with the plan at 0 damage and detonated anyway. Tell the owner to
  undo past the resolution rather than answering the pending prompt.

**Why:** a counter doing double duty as loop variable and phase selector has no
type-level guard, and the off-by-one only fires when a tier is exactly as wide as the
counter — rare enough to ship.

**How to apply:** when a card says something enters play *with* tokens, place them at the
`createAssetAt` site, not in a trailing `doStep`; keep a `doStep` counter meaning exactly
one thing, terminate at 0, and add a `null` bail-out. Related:
[[project_act_advance_side_dispatch]],
[[project_skilltest_option_messages_baked_early]].
