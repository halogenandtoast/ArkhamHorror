---
title: project_among_searched_cards_setup_guard
description: AmongSearchedCards reactions were suppressed during ALL setup searches; now only scenario-sourced ones are blocked
---

The `AmongSearchedCards` window matcher in `Arkham/Helpers/Window.hs` had a blanket
`not <$> getInSetup` guard (added in commit 15c8d2b3b3 to stop Surprising Find / Astounding
Revelation firing during Undimensioned & Unseen's setup `searchCollectionForRandom`). That guard
over-suppressed: a player-card-initiated search during setup (Whitton Greene searching her deck
when the starting location is revealed during scenario setup) also got blocked, so Astounding
Revelation (`06023`) / Surprising Find (1) / Occult Evidence / Dead Ends / Shocking Discovery
never offered their "among searched cards" reactions.

Fix: during setup, allow the window when `sourceMatches search'.searchSource SourceIsPlayerCard`
(asset/event/skill/investigator/ability sources → True; scenario/encounter sources → False).
`RevealLocation` is NOT setup-skippable (`isSetupSkippableWindow` only skips PutLocationIntoPlay /
LocationEntersPlay / clue placement), which is why Whitton fired but the search reaction didn't —
that asymmetry was the smoking gun. Issue #4957.

Note: can't `arkham-replay --undo` verify — the bug is during scenario setup, upstream of the
DeckAnswer boundary undo can't cross. Verified via unit test that flips `inSetupL` and runs a
player-sourced vs scenario-sourced search. See [[project_basic_attack_enemy_source]] for the
SourceIsPlayerCard distinction used elsewhere.
