---
name: project_alternate_printings_distinct_carddefs
description: "cdAlternateCardCodes makes each printing its OWN CardDef (structural Eq says \"different card\"), so allPlayerCards-derived pools double-count reprints; dedupe with canonicalCardCode"
metadata: 
  node_type: memory
  type: project
  originSessionId: 4af28b19-e1ec-4edf-88d3-ef29230ed06a
  modified: 2026-07-28T07:38:12.646Z
---

`toCardCodePairs` (`Arkham/Card/CardDef.hs`) registers every entry in
`cdAlternateCardCodes` as a **separate map entry holding a mutated `CardDef`** —
`cdCardCode`, `cdArt`, `cdAlternateCardCodes`, `cdSkills`, `cdErrata` are all rewritten
per printing. `CardDef` derives a **structural** `Eq`, so two printings of one card are
NOT equal.

Two consequences:

1. Any list derived from `toList allPlayerCards` (e.g. `allBasicWeaknesses` in
   `Arkham/PlayerCard.hs`) contains a reprinted card **once per printing**. Uniform
   sampling over such a list silently weights reprints 2–3×. Mob Enforcer is `01101`
   (Core) + `01601` (Revised Core); Chapter 2 adds a third for some
   (`12097`/`12100`/`12101`).
2. Dedup by `CardDef` equality (or by `toCardDef`) does **not** collapse printings.

Fix helper: `canonicalCardCode :: CardDef -> CardCode` in `Arkham/Card/CardDef.hs` —
`foldl' min (cdCardCode c) (cdAlternateCardCodes c)`. Stable across printings because
`toCardCodePairs` preserves the full code set on every copy.

**Why:** #5264 — Boon of the Morrígan offered Mob Enforcer twice among its three draws,
because its distinctness check used `toCardDef card \`elem\` map toCardDef acc`.

**Why:** #5346 — the same trap one layer down, on raw *card codes* rather than `CardDef`s.
`ReloadDecks` reconciles a saved campaign deck against `campaignStoryCards` at every
scenario start, dropping the deck's copy of anything that is already a story card. It
compared `card.cardCode` against `map toCardCode storyCards`, so when the engine rolled
revised-core Stubborn Detective (`01603`) as the random basic weakness and the player
hand-added the core printing (`01103`) to their ArkhamDB list, neither was dropped and the
deck grew by one every reload. Now `partitionReloadedDeck` in `Arkham/Helpers/Deck.hs`,
shared by `Campaign/Runner.hs` `ReloadDecks` and `Helpers/Campaign.hs` `getCurrentDeck`.

**How to apply:**
- Dedupe/group by `canonicalCardCode`, never by `CardDef` `Eq` or `toCardDef`, whenever
  the defs may come from different printings.
- This applies to bare `CardCode` comparisons too, not just `CardDef` `Eq`. Anywhere a
  player-supplied decklist is matched against engine-generated cards (deck reconciliation,
  "already owned" checks, card limits), the two sides can be different printings.
- **Group AFTER filtering, not before.** An arkham.build `card_pool` of `cycle:rcore` or
  `cycle:core_ch2` permits *only* the alternate printing; collapsing to the canonical code
  up front then filtering would drop the card entirely.
- To keep art variety while fixing the weighting, sample twice: pick the group, then pick
  the printing (see `randomBasicWeaknessSamplingGroups` / `sampleRandomBasicWeakness` in
  `Arkham/Decklist/RandomBasicWeakness.hs`).
- Note `minimum` from `Arkham.Prelude` is ClassyPrelude's `NonNull`-constrained one — it
  does not accept a `NonEmpty`. Use `foldl' min`, or `minimumEx` on a list.

Related: [[project_enemyis_loose_ab_crossmatch]] (CardCode's `Eq` is loose across a/b
sides while its `Ord` is exact — fine as a grouping key, but don't confuse the two).
