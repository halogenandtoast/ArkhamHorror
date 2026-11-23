---
title: A slot-icon asset in a threat area occupies that slot
date_added: 2026-11-23
source: ArkhamDB Rules Reference "Slots" + Grimoire v1.0 "Slots" (identical wording); confirmed internally against #6051
affects:
  - Back Injury
  - Maimed Hand
  - The Silver Moth
  - Vow of Dryztelech
  - slots
  - threat area
---

# A slot-icon asset in a threat area occupies that slot

> Each asset in an investigator's play area **or threat area** with a slot symbol is held in
> a slot of that type.

> If playing or gaining control of an asset would put an investigator above his or her slot
> limit for that type of asset, the investigator must choose and discard other assets under
> his or her control simultaneously with the new asset entering the slot.

So putting a slot-icon weakness asset into your threat area pays the full slot cost, and the
overflow discard is mandatory. It is not a bug and there is no exemption for the threat area.

The Feast of Hemlock Vale injury weaknesses are built on exactly this: each one prints a slot
icon, cannot leave play except by its own `[action] [action]` ability, and so permanently
costs the bearer a slot.

| card | slot it eats |
| --- | --- |
| Maimed Hand (`10135`) | 1 hand |
| Back Injury (`10136`) | body |
| The Silver Moth (`10137`) | accessory |
| Vow of Dryztelech (`10138`) | arcane |

The way to **not** pay the slot is the other half of each Revelation — take 1 damage (or
1 horror) and shuffle the card into your deck.

Worked example from #6051: Back Injury put into play in the threat area while Flamethrower (5)
(`04305`, `[body][hand][hand]`) was in play. Flamethrower was the only body-slot asset, so the
engine's single forced option — discard the Flamethrower — was the only legal outcome.

## Affected cards / systems

- Back Injury (`10136`) — `backend/arkham-api/library/Arkham/Asset/Assets/BackInjury.hs`
- Maimed Hand (`10135`) — `backend/arkham-api/library/Arkham/Asset/Assets/MaimedHand.hs`
- The Silver Moth (`10137`) — `backend/arkham-api/library/Arkham/Asset/Assets/TheSilverMoth.hs`
- Vow of Dryztelech (`10138`) — `backend/arkham-api/library/Arkham/Asset/Assets/VowOfDrzytelech.hs`
- `cdSlots` on all four — `backend/arkham-api/library/Arkham/Asset/Cards/TheFeastOfHemlockVale.hs`
- `InvestigatorAdjustAssetSlots_` / `RefillSlots` —
  `backend/arkham-api/library/Arkham/Investigator/Runner.hs`

## Implementation status

- **the slot cost itself**: ✅ already correct. `place attrs (InThreatArea iid)` expands to
  `InvestigatorAdjustAssetSlots_` + `Do (InvestigatorPlayAsset_ …)`, so a threat-area
  placement goes through the same slot machinery as a play-area one and raises the overflow
  discard prompt. Verified step-by-step in the #6051 export (steps 1222→1223).
- **`cdSlots` on all four cards**: ✅ match the printed slot icons in `mcp/data/cards.json`.
- **the discard ability**: ✏️ Back Injury and Maimed Hand gated their `[action] [action]:
  Discard …` on `OnSameLocation`, which let **any** investigator at the bearer's location
  spend 2 actions discarding someone else's weakness. Both now use
  `controlled_ attrs 1 doubleActionAbility`, matching The Silver Moth and Vow of Dryztelech,
  which were already right.

  The `OnSameLocation` idiom comes from FAQ 1.2, whose source list is "all **encounter
  cards** in the threat area of any investigator at that location"
  (`../engine-gotchas/project_threat_area_action_is_not_bearer_only.md`). It does not reach
  these four, because the Weakness rule splits on cardtype, not on placement:

  > Weaknesses with an encounter cardtype are, like other encounter cards, not controlled by
  > any player. **Weaknesses with a player cardtype are controlled by their bearer.**
  >
  > — `../rules/glossary/weakness.md`

  A weakness **treachery** in a threat area (Internal Injury, Dreams of R'lyeh) is an
  encounter card once in play, so `OnSameLocation` is right there. A weakness **asset**
  (these four) is a player card controlled by its bearer, so its ability is reachable only
  through "cards in play under your control" — bearer-only, `controlled_`.
