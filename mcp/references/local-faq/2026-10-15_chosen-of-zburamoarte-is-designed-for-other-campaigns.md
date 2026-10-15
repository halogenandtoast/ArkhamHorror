---
title: The Chosen of Zburamoarte is designed for campaigns other than Children of Blood
date_added: 2026-10-15
source: rules Q&A relayed by the user (designer answer; venue not recorded — confirm attribution)
affects:
  - Chosen of Zburamoarte (Fighting the Hunger) (13093a)
  - Chosen of Zburamoarte (Compelled to Feed) (13093b)
  - Children of Blood
  - blood tokens
---

# The Chosen of Zburamoarte is designed for campaigns other than Children of Blood

**Q: How does the story reward card (The Chosen of Zburamoarte) for Children of Blood
work in Children of Blood?**

A:

> The Chosen of Zburamoarte asset was generally designed with the intention to integrate
> blood tokens into campaigns outside of Children of Blood; using it while replaying
> Children of Blood will greatly skew the balance of that campaign, as it seals blood
> tokens revealed by any investigator onto it.

Three things follow:

1. The card's seal is **not** scoped to its controller. Any investigator revealing a
   blood token feeds it, which is exactly why it is unbalanced in a campaign that already
   runs on blood tokens.
2. Nothing stops a player taking it back into Children of Blood. The skew is the expected
   consequence, not a bug — do not add a guard against it.
3. Its purpose is the reverse direction: carrying blood tokens *out* into another
   campaign, which the Fighting the Hunger side does by adding up to 5 blood tokens to
   the chaos bag at the start of the game.

## Affected cards / systems

- Chosen of Zburamoarte (Fighting the Hunger) (`13093a`) — `backend/arkham-api/library/Arkham/Asset/Assets/ChosenOfZburamoarteFightingTheHunger.hs`
- Chosen of Zburamoarte (Compelled to Feed) (`13093b`) — `backend/arkham-api/library/Arkham/Asset/Assets/ChosenOfZburamoarteCompelledToFeed.hs`
- Card defs — `backend/arkham-api/library/Arkham/Asset/Cards/ChildrenOfBlood.hs:41-58`
  (a `permanent` unique story asset, traits Condition/Blight/Reward, two sides via
  `cdOtherSide`)

## Implementation status

- **Both sides**: ✅ already match the ruling — no change. Each side's
  `RevealChaosToken _ iid token` handler is keyed on the *revealing* investigator with
  no controller check, so a blood token revealed by anyone at the table is sealed on the
  card (`afterSkillTestQuiet $ sealChaosToken iid attrs token`). Compelled to Feed also
  flips back to Fighting the Hunger once its last sealed blood token is released.
- **Blood tokens into other campaigns**: ✅ Fighting the Hunger's Forced
  `GameBegins #when` ability adds `min 5 getRemainingBloodTokens` blood tokens for the
  game, which is the integration the ruling describes.
- This entry exists to stop a future pass "fixing" the missing controller check on the
  reveal, or adding a Children-of-Blood exclusion. Both are intentional.
