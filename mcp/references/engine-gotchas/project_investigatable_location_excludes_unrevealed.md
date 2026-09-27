---
name: project_investigatable_location_excludes_unrevealed
description: "InvestigatableLocation is false for every unrevealed location (LocationShroud returns Nothing pre-reveal), so move-then-investigate cards like Duke were gated off their own move target"
metadata: 
  node_type: memory
  type: project
  originSessionId: 63c69607-12ab-42e9-bf1a-3ba719a38a2a
  modified: 2026-09-22T05:45:13.651Z
---

`Matcher.InvestigatableLocation` (`Game.hs`) is `fieldMap LocationShroud isJust` +
no `CannotInvestigate`, and `LocationShroud` returns **Nothing for any unrevealed
location** (`Game.hs`, `if isRevealed l && isJust locationShroud`) even though the
attrs hold the printed shroud. So it means "investigatable *right now*", never
"investigatable once I'm standing there".

That breaks cards that MOVE you before investigating. Duke ("you may move to a
connecting location immediately before investigating") gated ability 2 on
`oneOf [YourLocation, CanMoveToLocation …] <> InvestigatableLocation`; parked at
Your House under a Locked Door's `CannotInvestigate` with only an unrevealed
Rivertown connecting, both candidates failed and the ability vanished from the
question entirely.

**How to apply:** for a move-then-investigate card use
`PotentiallyInvestigatableLocation` (printed shroud + no `CannotInvestigate`) on
the move branch and plain `InvestigatableLocation` on the "here" branch, joined
with `oneOf`, then pass the whole thing to `withInvestigationTargetsMatching`
(`Ability.hs`) — `withInvestigationTargets` ANDs `InvestigatableLocation` onto
whatever you give it, which kills the move branch. The stored
`InvestigateTargets` matcher is re-selected in `Helpers/Ability.hs`
`getCanAffordAbilityCost` to price additional investigate costs, so criterion and
cost must use the *same* matcher or the ability passes its criteria and then
fails affordability. Remote-investigate cards (Dowsing Rod, In the Know (1),
Local Map, Pocket Telescope) do NOT want this — they never move you, so
requiring a revealed location is correct.

Also match the criterion to the handler: Duke's said `Anywhere` while the handler
used `getAccessibleLocations`, so it was satisfiable by any location on the
board. `CanMoveToLocation You src (AccessibleFrom ForMovement YourLocation)` is
the pairing that agrees (prior art: Ruby Standish, Master Thief). `ForMovement`
comes from `Arkham.ForMovement`, which `Asset.Import.Lifted` does not re-export.

Related: [[project_skilltestat_matches_target_location]],
[[project_candiscoverclues_permission_not_presence]].
