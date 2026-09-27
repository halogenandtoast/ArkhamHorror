---
name: project_single_card_shuffle_empty_deck
description: shuffleCardsIntoDeck silently no-ops for a SINGLE card into an empty investigator/encounter deck (FAQ 1.13) — cards that shuffle a group must push one grouped message
metadata: 
  node_type: memory
  type: project
  originSessionId: c1f53430-2bb1-4888-b470-6c03e02eff80
  modified: 2026-07-30T12:27:30.372Z
---

`getCanShuffleIn` (`Arkham/Helpers/Shuffle.hs`) enforces FAQ **(1.13)**: *"A single card cannot be shuffled into an empty player deck or encounter deck via card effect."* `preventsShuffleIntoEmptyDeck` covers `InvestigatorDeck`, `EncounterDeck`, `EncounterDeckByKey`; the `CanShuffleIn [a]` instance only applies the empty-deck check when the list has **exactly one** element — 2+ cards are always allowed.

So a card that shuffles N chosen cards **as a group** must emit ONE `ShuffleCardsIntoDeck`. Doing it per-pick inside `chooseUpToNM_ ... (shuffleCardsIntoDeck iid . only)` silently drops every shuffle when the deck is empty, because `whenCanShuffleIn` is evaluated while the choice messages are being captured. The failure is invisible: no error, no log line, the picks just do nothing.

**Why:** Respite (2) issue #5302 — Miguel with an empty deck chose 3 level-0 cards, nothing was shuffled, and the follow-up "Draw 1 card" then hit the empty-deck rule and reshuffled the *whole* 24-card discard plus 1 horror. The player read that as "the draw resolved before the shuffle."

**How to apply:** For "choose up to N cards and shuffle them into your deck", record the picks (`handleTarget` → `HandleTargetChoice` → `setMeta (cid : chosen)`) and do a single `shuffleCardsIntoDeck` in a `Do msg` handler queued behind the `chooseUpToNM_` Ask — that also puts the shuffle before any later `drawCards`. See `Arkham/Event/Events/Respite2.hs`. `Scenario/Scenarios/TheSilentHeath.hs` (~156, 164) still has the per-pick shape against the encounter deck. Related: [[project_engine_gotchas_repo_mirror]]
