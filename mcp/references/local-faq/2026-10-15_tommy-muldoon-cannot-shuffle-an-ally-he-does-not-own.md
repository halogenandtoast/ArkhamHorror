---
title: Tommy Muldoon cannot shuffle in an ally he does not own, so it reaches the victory display
date_added: 2026-10-15
source: rules Q&A relayed by the user (designer answer; venue not recorded — confirm attribution)
affects:
  - Tommy Muldoon (06001)
  - Maria Rivera (Lost Pilgrim) (11568)
  - asset defeat / victory display
  - card ownership vs. control
---

# Tommy Muldoon cannot shuffle in an ally he does not own, so it reaches the victory display

**Q: How does Chapter 1 Tommy killing a victory 0 ally work? Does it just go back in
his deck because it's not discarded, or do the story asset rules apply and it goes to
the victory display but he can get the money for it?**

A:

> If Tommy 1 owns and controls a victory 0 ally that's then defeated, he could trigger
> his ability to gain resources and shuffle it into his deck, and it would not be added
> to the victory display. If he instead controls but does not own that ally, he could
> trigger his ability and gain resources, but could not shuffle it into his deck, nor
> could it be shuffled into the encounter deck, so it would then be added to the victory
> display. (This is assuming the ally is owned by the scenario and does not have an
> encounter card back.)

Two independent halves:

1. **Owned and controlled.** "Shuffle that asset into your deck" happens, the card never
   reaches a discard pile or the victory display, and its victory points are lost for
   the scenario. Tommy's resource gain is unaffected.
2. **Controlled but not owned.** The shuffle has no legal destination — not Tommy's
   deck (he does not own the card) and not the encounter deck (a scenario-owned story
   asset with no encounter back is not an encounter card) — so it is skipped. The
   resource gain still happens, and the defeat then resolves normally, which for a
   victory card means the victory display.

Ownership, not control, is what decides this. The same reasoning applies to an ally
owned by *another investigator* that Tommy has taken control of: "your deck" is Tommy's
deck, so the shuffle is still illegal.

## Affected cards / systems

- Tommy Muldoon (`06001`) — `backend/arkham-api/library/Arkham/Investigator/Cards/TommyMuldoon.hs`
- Maria Rivera (Lost Pilgrim) (`11568`) — the one printed Victory 0 ally, a
  scenario-owned story asset from The Apiary:
  `backend/arkham-api/library/Arkham/Asset/Assets/MariaRiveraLostPilgrim.hs`. Her own
  **Forced** ability ("If Maria Rivera is defeated: Add her to the victory display")
  rides the same `AssetDefeated #when` window as Tommy's reaction.
- Engine: `Do (AssetDefeated ...)` routes a defeated asset with
  `cdVictoryPoints` to `AddToVictory` —
  `backend/arkham-api/library/Arkham/Asset/Runner.hs:443`
- Engine: `cancelAssetLeavePlay` strips that whole frame out of the queue —
  `backend/arkham-api/library/Arkham/Message/Lifted.hs:2900`
- Engine: `AssetOwner` is `Nothing` for anything the scenario created
  (`toCardOwner`), and `Just iid` for a card stamped on deck construction
  (`Arkham/Investigator/Runner/Card.hs:986`)

## Implementation status

- **Tommy Muldoon (`06001`)**: ✏️ updated. Ability 1 used to run
  `cancelAssetLeavePlay asset` and `shuffleIntoDeck iid asset` unconditionally, so a
  scenario-owned ally was pulled out of its own defeat *and* shuffled into Tommy's deck —
  wrong on both counts (no victory display, and a story card in a player deck). It now
  reads `AssetOwner` and only cancels the leave-play and shuffles when the card is his,
  and when he can actually manipulate his deck (`can.shuffle.deck`). Everything else
  falls through to `Do (AssetDefeated ...)`, which adds a victory card to the victory
  display and otherwise discards.
- **Resource gain**: ✅ already matched — it is guarded only by
  `can.gain.resources` and runs either way.
- **Maria Rivera (`11568`)**: ✅ no change. Her Forced ability adds her to the victory
  display on its own; with Tommy no longer cancelling her defeat, both abilities now
  agree.

## Open question

Tommy's reaction window is narrowed to `oneOf [AssetWithDamage, AssetWithHorror]`, but
his printed text has no such condition ("Gain X resources" where X may be 0, then shuffle
it in). An ally defeated with no damage or horror on it — defeated by a "defeat that
asset" effect rather than by damage — is therefore never offered the ability at all, and
so cannot be shuffled into his deck. That is a separate question from this ruling and was
left alone.
