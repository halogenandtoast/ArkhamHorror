---
title: project_playerwindow_active_investigator_stale
description: During-turn action window playability must wrap getPlayableCards in asIfTurn; active-investigator pointer can lag the turn owner
---

`gameActiveInvestigatorId` is distinct from `gameTurnPlayerInvestigatorId`. `runPreGameMessage` repoints active to the drawer/target on `DrawCards`/`ForInvestigator` (Game/Runner.hs ~3703-3708). A card that makes ANOTHER investigator draw/gain last (e.g. Stand Together(3)) leaves active pointing at that other investigator.

`RunMessage Game` (Game/Runner.hs ~3684) dispatches entities FIRST (`entitiesL`) and `runGameMessage` LAST. So for a `PlayerWindow iid` message, `handlePlayerWindow` (Investigator/Runner/Action.hs) builds the action window BEFORE the game-level `PlayerWindow` handler resets active to `iid`. Thus playability is evaluated with the stale active investigator.

`handlePlayerWindow` wraps `getActions` in `asIfTurn iid` (= `asActive`, sets active in the Reader) but originally did NOT wrap `getPlayableCards`. Result: abilities evaluate `YetToTakeTurn`/active-relative criteria correctly, but event playability does not — non-fast events whose criteria reference the active investigator (e.g. Guidance's `affectsOthers … YetToTakeTurn`) get wrongly dropped, while events keyed only on "You" (replaced via `replaceYouMatcher iid`, e.g. Emergency Cache) survive. Fix: wrap `getPlayableCards` in `asIfTurn iid` too (issue #4886).

Debugging note: the buggy window is serialized with the SETTLED state (active already reset to Carson, modifier cache settled) but a STALE stored question — so `arkham-replay` draining/re-asking the identical state offers the card again, which is the "undo and it works" signature. See [[project_action_diff_snapshot]].
