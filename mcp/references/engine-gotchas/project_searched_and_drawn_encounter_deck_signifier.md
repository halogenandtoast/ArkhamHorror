---
title: project_searched_and_drawn_encounter_deck_signifier
description: "Search-and-draw encounter effects open the DrawCard window with the EncounterDiscard signifier, so any window declared `EncounterDeck` silently misses them — use AnyDeck unless the card literally says \"from the encounter deck\""
---

`FoundAndDrewEncounterCard iid cardSource card` (Scenario/Runner.hs ~1547-1567) maps the card's origin zone
to a `Deck.DeckSignifier` before handing off to the normal draw path:

| `EncounterCardSource` | signifier |
| --- | --- |
| `FromEncounterDeck` | `Deck.EncounterDeck` |
| `FromKeyedEncounterDeck k` | `Deck.EncounterDeckByKey k` |
| `FromDiscard` | **`Deck.EncounterDiscard`** |
| anything else | `Nothing` → defaults to `Deck.EncounterDeck` in Game/Runner.hs |

It then pushes `InvestigatorDrewEncounterCardFrom iid card signifier`, which opens
`Window.DrawCard iid card <signifier>` (Game/Runner.hs ~3362).

`deckMatch` (Helpers/Deck.hs:112-113) treats `Matcher.EncounterDeck` as an **exact** equality test against
`Deck.EncounterDeck`. So a reaction whose window is `DrawCard #when You (...) EncounterDeck` never fires for a
card that a search pulled out of the encounter **discard** — the ability is filtered out of the window with no
error and no ask.

Issue #5299: *The Final Countdown* (agenda `05328`, Before the Black Throne) says "search the encounter deck
and discard pile for a copy of **Daemonic Piping** and draw it". The last copy was in the discard, so
**Erynn MacAoidh** (`54041`) could not cancel its revelation, the third copy entered play, and Piper of
Azathoth spawned. The same player had cancelled an earlier copy fine — that one came off the top of the deck.

**How to apply:** default a cancel/react-to-draw window to `AnyDeck`. Only use `EncounterDeck` when the printed
text actually says "from the encounter deck" — of the whole family, only **Jerome Davids** (`05259`) does.
Fixed in #5299: Erynn MacAoidh, Nine of Rods (3) (`54009`), Ravenous Myconid: Sentient Strain (4) (`10059`).
Already correct: Laudanum, all three Sacred Oaths, Ikiaq, Dark Insight.

Widening is safe: `AnyDeck` short-circuits to `True`, and the `DrawCard` window only exists when a real draw
happens, so it cannot fire on put-into-play-without-drawing scenario effects.

**Verifying this class of bug from an export:** `--undo 1` is not enough. The saved queue for a step is the
queue *remaining after* that step's question was asked, so resuming drops the answer and its question — here it
skipped the search entirely and drained to the normal mythos draw. Re-inject the draw instead:
`--undo 1 --answers '[{"tag":"Raw","contents":{"tag":"FoundAndDrewEncounterCard","contents":[iid,{"tag":"FromDiscard"},<encounter card>]}}]'`.
A `Raw` answer with a question pending resolves to `[message, AskMap gameQuestion]` (Entity/Answer.hs ~491), so
the injected message runs first and the old question is re-parked behind it.

Related: [[project_window_entry_tick_timing]], [[project_among_searched_cards_setup_guard]],
[[project_replay_cached_question_needs_undo]].
