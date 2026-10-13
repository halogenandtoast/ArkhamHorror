---
title: project_permanent_keyword_was_not_a_discard_guard
description: The central asset Discard guard checked the CannotLeavePlay modifier but not the permanent keyword, so any discard effect missing DiscardableAsset could discard a permanent
---

Permanent is categorical — "A card with the permanent keyword cannot leave play (except by
elimination)" (ArkhamDB Rules Reference and Grimoire glossary both). The engine enforced that
in exactly one place: the `DiscardableAsset` matcher (`Game.hs:3431`, via `canBeDiscarded` at
`:3257`, which ANDs `assetCanLeavePlayByNormalMeans`, `not . cdPermanent . toCardDef` and
`AssetWithoutModifier CannotLeavePlay`). A card that forgot to include it in its select could
discard a permanent outright.

The central `Discard` clause in `Asset/Runner.hs` looked like a backstop but was not one — it
guarded `hasModifier CannotLeavePlay` only, with a deliberate `isSource a source` escape so a
card that prints its own way out of play can still discard itself. The **keyword** fell
through.

That stayed invisible for years because no permanent asset accumulated damage or horror. The
Drowned City Task weaknesses (`11753a`–`11760a`) broke that: all permanent, all designed to
pile up damage, horror, resources, uses and clues on themselves. Tommy Muldoon's Rookie Mistake
("Discard each asset you control with damage or horror on it. If no assets are discarded by
this effect, shuffle Rookie Mistake back into your deck") selected
`assetControlledBy iid <> oneOf [AssetWithDamage, AssetWithHorror]` with no
`DiscardableAsset`, so it discarded Toe the Line (`11755a`) with the 4 damage Toe the Line's
own Forced had placed there — and, because the permanent counted as "discarded by this
effect", Rookie Mistake went to the discard instead of back to the deck (#5841).

Both halves were fixed: the card now ANDs `DiscardableAsset`, and the central guard is
`orM [pure (toCardDef a).permanent, a `hasModifier` CannotLeavePlay]`, keeping the self-source
escape. Surveyed every other permanent asset first — none leaves play via a foreign-source
`Discard`: Expedited Ticket (`09765`) uses `Exile`, the two-sided permanents (`08011a/b`,
`10005/b`, the Tasks) use flips, and `handleInvestigatorEliminated`
(`Investigator/Runner/Damage.hs:261`) never pushes `Discard` at controlled assets.

Two consequences worth remembering:

- A permanent no longer satisfies an "if any/no assets were discarded" rider. Select with
  `DiscardableAsset` rather than discarding and counting afterwards, or the rider reads the
  wrong branch even once the discard itself is blocked.
- The central guard now **silently** no-ops a foreign-source discard of a permanent. If a
  future card is genuinely meant to remove one, it has to source the removal from the card
  itself or use a different message.

A story asset discarded this way lands in the SCENARIO discard, not the investigator's:
in an export the tell was `gameMode.That.discard[0].cardCode = c11755a` plus a
`gameTombstones.assets` entry whose `placement` still read `InPlayArea`.

Related: [[project_remove_from_game_skips_leaveplay]], [[project_uniqueness_check_was_asset_only]].
