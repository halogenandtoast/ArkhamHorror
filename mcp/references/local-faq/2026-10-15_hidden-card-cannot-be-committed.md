---
title: A Hidden card cannot be committed to a skill test
date_added: 2026-10-15
source: rules Q&A relayed by the user (designer answer; venue not recorded — confirm attribution)
affects:
  - Watcher from Another Dimension (06017)
  - Friends in Low Places (09060)
  - Possession (Traitorous) (03340)
  - hidden keyword
  - commit restrictions
---

# A Hidden card cannot be committed to a skill test

**Q: If I am playing Preston and I choose Monster on my Friends in Low Places with
Bolstering and Helpful, can Patrice commit Watcher from Another Dimension?**

A:

> Because Watcher from Another Dimension has the Hidden keyword, it cannot be removed
> from Patrice's hand except via the ability on it; Friends in Low Places would not get
> around this.

This is the hidden keyword's last bullet in the rules reference — "a hidden card counts
toward your hand size, but it cannot leave your hand by any means except those described
on the card" — applied to committing. Note what it does *not* turn on:

- It does not matter that the card reached hand by a search rather than by its own
  Revelation. The keyword is printed on the card and applies while the card is in hand.
- It does not matter that Bolstering gave it a [wild] icon. Having a matching icon is
  necessary to commit, not sufficient; the hidden keyword is a separate, flat prohibition.
- The exception is genuinely "described on the card": Possession (Traitorous) is hidden
  *and* prints permission to commit itself, so it stays committable.

## Affected cards / systems

- Watcher from Another Dimension (`06017`) — Patrice's signature weakness, a player
  [[Monster]] enemy card with `Keyword.Hidden`
  (`backend/arkham-api/library/Arkham/Enemy/CardDefs/TheDreamEaters.hs:20-32`,
  `.../Enemy/Cards/WatcherFromAnotherDimension.hs`)
- Friends in Low Places (`09060`) — `backend/arkham-api/library/Arkham/Event/Events/FriendsInLowPlaces.hs`
  (Helpful points the look at another investigator, Bolstering adds
  `AddSkillIcons [#wild]` to each card added to hand)
- Possession (Traitorous) (`03340`) — the hidden card that may be committed, via
  `cdCommitRestrictions = [CommittableTreachery]`
  (`backend/arkham-api/library/Arkham/Treachery/CardDefs/ThePathToCarcosa/DimCarcosa.hs:34`)
- Engine: `getIsCommittable` — `backend/arkham-api/library/Arkham/Helpers/SkillTest.hs`
- Already honoured elsewhere: `DiscardableCard` is
  `NonWeakness <> CardWithoutKeyword Hidden` — `backend/arkham-api/library/Arkham/Card.hs:304`

## Implementation status

- **Engine (`getIsCommittable`)**: ✏️ fixed. Nothing in the commit path looked at the
  hidden keyword. A hidden card normally fails the icon guard — `iconsForCard` returns
  `cdSkills`, which is empty for an enemy or treachery, and `cdCanCommitWhenNoIcons` is
  `False` — so the hole was invisible until something granted icons. Bolstering does
  exactly that, and Watcher from Another Dimension was then committable. The check now
  sits beside the `CannotCommitCards` guard, so it covers both the hand and the
  `CanCommitToSkillTestsAsIfInHand` cards:

  ```haskell
  guard $ Hidden `notElem` cdKeywords cdef || CommittableTreachery `elem` cdCommitRestrictions cdef
  ```

- **Possession (Traitorous) (`03340`)**: ✅ unaffected. It is an encounter card, so it
  was already gated on `CommittableTreachery` in the `EncounterCard` branch, and the new
  guard carries the same exemption.
- **Friends in Low Places (`09060`)**: ✅ no change. Finding and adding the card to hand
  is legal — the card is a [[Monster]] and a legal target of the look — and Bolstering
  still grants its [wild] icon. Only the commit is refused.
- **Scope**: this covers committing only. The keyword's wider "cannot leave your hand"
  clause is still enforced piecemeal (`DiscardableCard` for discard-from-hand effects);
  a card that can *play* or otherwise move a hidden card out of hand has not been
  audited.
