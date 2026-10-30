---
title: project_threat_area_action_is_not_bearer_only
description: An [action] on a threat-area encounter card is usable by ANY investigator at that location (FAQ 1.2), so `InThreatAreaOf You` under-restricts unless the card prints "in your threat area"
---

Rules Reference → Ability → Triggered Abilities carries the FAQ "Game Play" 1.2 list of sources an
investigator may trigger abilities from, and it includes

> all encounter cards in the threat area of **any** investigator at that location.

So `restricted a n (InThreatAreaOf You)` on a treachery's `[action]` is **narrower than the card**
whenever the printed text does not itself say "in your threat area". Nothing errors — the ability
is simply absent from the other seat's choice list, which looks like the card not working at all.

Predictive Algorithm (`:dark-matter:052`, "*[action] Choose and discard 2 cards from your hand:
Discard Predictive Algorithm.*") hit this: Jacqueline Fine at Skids O'Toole's location was offered
the ability on her own copy and not on his (#5932). The Colors of Space (`:dark-matter:073`,
"*[action] Take 1 horror: Discard The Colors of Space.*") had the identical defect.

The idiom is the one `RadioactiveDecay.hs` already documents:

```haskell
restricted a 1 (OnSameLocation <> InThreatAreaOf Anyone) $ actionAbilityWithCost ...
```

`OnSameLocation` is the real gate — `Criteria.OnSameLocation` resolves a `TreacherySource` through
`Helpers/Placement.hs`'s `onSameLocation`, whose `InThreatArea iid'` arm returns `True` when
`iid == iid'` *or* when both investigators share a location, so this is a strict **superset** of
`InThreatAreaOf You` and the bearer never loses access. `InThreatAreaOf Anyone` is what stops
`OnSameLocation` from also offering the ability while that copy sits face down or placed elsewhere.

**The printed text is the arbiter, so this is not a blanket sweep.** Night Terrors (`06098`) prints
"*[action] **If Night Terrors is in your threat area**: Test [willpower] (4)*" — there
`InThreatAreaOf You` is correct, and the ~13 other treacheries in the repo that gate an `[action]`
that way have to be read one at a time rather than mass-rewritten.

"You" inside the ability's own cost and effect still means the activating investigator, so widening
the criteria alone is the whole fix: `HandDiscardCost`/`HorrorCost … YouTarget` already bill the
user, and `toDiscardBy iid` already credits them. Do **not** widen the card's constant ability at
the same time — "*You get -1 to each of your skills…*" belongs to the bearer and stays keyed on
`a.placement`'s `InThreatArea iid`.

Verifying this needs no `--undo`: the bug is a missing option in a *pending* question, so replaying
the export straight through and diffing the choice list is the evidence (19 → 20 choices for
Jacqueline, the new one an `AbilityLabel` on the other seat's treachery id).
