---
title: A token returned by Rex's Curse still resolves its delayed success/failure effect
date_added: 2026-10-01
source: Maxine Newman (FFG) rules clarification, relayed by the user on GitHub issue #5800
affects:
  - Rex's Curse
  - Rex's Curse (Advanced)
  - ReturnSkillTestRevealedChaosTokens
  - ResolveChaosToken
  - skill test timing (ST.3–ST.7)
  - The Pit of Despair
---

# A token returned by Rex's Curse still resolves its delayed success/failure effect

> This is a bit of a tricky situation so I will outline it step by step, as best
> I can. =)
>
> Rex's Curse triggers during Step 6, when you determine success/failure of the
> skill test. By that time, some chaos token effects will have already
> triggered. Chaos token effects that say "if you succeed" or "if you fail" will
> not have triggered yet, as Rex's Curse interrupts the timing of those effects,
> but since Rex's Curse does not cancel or ignore the token (like Wendy's Amulet
> or Grotesque Statue does), the token will have created a delayed effect that
> will trigger during Step 7, regardless of whether the token is returned to the
> bag or not. Chaos token effects that simply have an effect (like "Place 1 doom
> on the nearest Cultist enemy," for example) trigger during Step 4, before
> Rex's Curse triggers.
>
> After you return the chaos token and draw a new chaos token from Rex's Curse,
> the sequence returns to Step 3, and you should follow the sequence in order as
> normal.
>
> So, in your example:
>
> You draw the cultist token during step 3. During step 4, you place 1 doom on
> the nearest cultist, as part of the token's effect.
> When you would pass the test (during step 6), you return the revealed chaos
> token to the bag and reveal a new chaos token. This returns you to step 3 of
> the sequence.
> You draw another cultist token! Poor Rex. During step 4, you place another
> doom on the nearest cultist.
> Step 6 rolls around again and you pass the test, so Rex's Curse remains in
> play.
>
> Had you revealed a chaos token that says "If you succeed / if you fail" during
> your first reveal, it might go like this:
>
> You draw the hypothetical token during step 3. It says "if you fail, take 1
> horror." This creates a delayed effect that will deal you 1 horror during step
> 7 if you fail the test.
> When you would pass the test (during step 6), you return the revealed chaos
> token to the bag and reveal a new chaos token. This returns you to step 3 of
> the sequence.
> You draw another hypothetical token. This creates another delayed effect that
> will deal you 1 horror during step 7 if you fail.
> If you failed this hypothetical test, during step 7, you would take 2 horror
> (one from each of the tokens).
>
> — Maxine Newman

In short, three separate things fall out of this:

1. **Immediate (Step 4) effects** resolve once per reveal, so a redraw of the
   same symbol resolves it a second time.
2. **Delayed "if you succeed / if you fail" effects** survive the token going
   back in the bag. Two reveals of an "if you fail, take 1 horror" token means
   2 horror on a failed test.
3. **Modifiers do not survive.** The sequence returns to Step 3, so only the
   newly revealed token contributes its modifier to the skill value.

Rex's Curse is *not* a cancel. Compare Grotesque Statue (4), which acts at
`WouldRevealChaosToken #when` — before the reveal is ever recorded — and so
leaves no delayed effect behind.

## Affected cards / systems

- Rex's Curse (`02009`) — `backend/arkham-api/library/Arkham/Treachery/Cards/RexsCurse.hs`
- Rex's Curse (Advanced) (`90080`) — `backend/arkham-api/library/Arkham/Treachery/Cards/RexsCurseAdvanced.hs` (reveals an *additional* token rather than returning one, so nothing is returned)
- Engine: `ReturnSkillTestRevealedChaosTokens` — `backend/arkham-api/library/Arkham/SkillTest/Runner.hs:701`
- Engine: delayed-effect fan-out (`tokenSubscribers`) — `backend/arkham-api/library/Arkham/SkillTest/Runner.hs:492`, `:761`, `:824`, `:922`, `:993`
- Engine: skill value from tokens — `totalChaosTokenValues`, `backend/arkham-api/library/Arkham/Helpers/SkillTest.hs:461`
- Contrast (a real cancel): Grotesque Statue (4) (`01071`) — `backend/arkham-api/library/Arkham/Asset/Assets/GrotesqueStatue4.hs`
- The Pit of Despair (`07041`) Tablet, Cultist and Elder Thing effects — `backend/arkham-api/library/Arkham/Scenario/Scenarios/TheInnsmouthConspiracy/ThePitOfDespair.hs:120-143`

## Implementation status

- **Rex's Curse (`02009`)**: ✅ already matches the ruling on all three points — no change.
  - `ReturnSkillTestRevealedChaosTokens` clears only `setAsideChaosTokensL`,
    deliberately leaving `revealedChaosTokensL` and `resolvedChaosTokensL`
    populated. The returned token therefore stays in the `tokenSubscribers` list
    that `Do FailSkillTest` / `SkillTestResults` fan out, so its
    `FailedSkillTest _ _ _ (ChaosTokenTarget token) _ _` /
    `PassedSkillTest …` handler still fires at ST.7 (point 2).
  - A second reveal of the same face appends to `revealedChaosTokensL`, so both
    delayed effects fire (point 2's "2 horror" case).
  - `totalChaosTokenValues` sums `skillTestSetAsideChaosTokens`, which *is*
    cleared, so only the new token's modifier counts (point 3).
  - Immediate effects run from `ResolveChaosToken` during
    `ResolveChaosSymbolEffectsStep`, once per reveal (point 1).
- **`revealedChaosTokensCount`** intentionally keeps counting across the redraw,
  so `FirstChaosTokenRevealedThisSkillTest` (Correlate All Its Contents) does
  not treat the replacement token as the first reveal of the test.
- This entry ratifies the existing implementation. Commit `95843e8158` (Apr
  2024) removed the `revealedChaosTokensL`/`resolvedChaosTokensL` clear from
  `ReturnSkillTestRevealedChaosTokens` on purpose — **do not restore it.** It
  reads like a bug (issue #5800 was filed against exactly this behaviour: a
  Tablet that would have passed was returned, the redraw auto-failed, and the
  Tablet's "if you fail, take 1 horror" still applied) but it is correct.
