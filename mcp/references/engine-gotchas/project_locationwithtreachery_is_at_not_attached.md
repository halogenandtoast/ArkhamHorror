---
title: project_locationwithtreachery_is_at_not_attached
description: "`LocationWith(out)Treachery` means treachery AT the location, which includes threat-area placements, so an unconstrained `AnyTreachery` never means \"nothing attached here\""
---

`LocationWithoutTreachery` / `LocationWithTreachery` read as attachment predicates and are not.
`Game.hs:2614` implements them as

```haskell
LocationWithoutTreachery matcher -> flip filterM ls $ \l -> do
  selectNone $ treacheryAt (toId l) <> matcher
```

and `treacheryAt` is `TreacheryAt . LocationWithId`, whose arm (`Game.hs:1914`) resolves the
treachery's placement through `Helpers/Location.hs`'s `placementLocation`. That function maps

```haskell
InThreatArea iid           -> fieldMayJoin InvestigatorLocation iid
AttachedToInvestigator iid -> fieldMayJoin InvestigatorLocation iid
```

— i.e. **a treachery in an investigator's threat area counts as being at whatever location that
investigator is standing on.** Which is right for "treacheries at your location", and wrong for
every card that prints "attached to it".

The trap only fires when the inner matcher is unconstrained. Every other caller in the repo passes
`treacheryIs <card>` or `mapOneOf treacheryIs [...]` (Locked Door, Secret Door, Arcane Spotlight,
Dimensional Hypnosis, Stone Barrier, Polar Mirage, Antarctic Wind, Wall of Thorns, …), and a
threat-area treachery can never satisfy `treacheryIs Cards.lockedDoor`. Barricaded Door
(`:return-to-the-innsmouth-conspiracy:055`) was the one card reaching for `AnyTreachery`, for
printed text that really does say "the location with the most clues on it **that does not have a
treachery attached to it** (you choose in case of a tie)" — and so it silently excluded the
location of any investigator carrying a threat-area treachery (#5937).

Pin the placement down in the inner matcher instead:

```haskell
LocationWithMostClues $ LocationWithoutTreachery (TreacheryAttachedToLocation Anywhere)
```

`TreacheryAttachedToLocation` (`Matcher/Treachery.hs:47`) matches only the `AttachedToLocation`
arm, so the conjunction with `treacheryAt l` is exactly "attached to l".

**The failure mode is a missing choice, not an error.** `chooseOrRunOneM` runs a single candidate
directly, so over-filtering a tie down to one location presents no question at all — the engine
just places the card and moves on. In #5937 Shoreward Slums and First National Grocery were tied
at 3 clues; Silas Marsh stood on the Grocery holding Innsmouth Look (`07106`) **and Indebted
(`02037`)**, and Indebted is a *permanent* weakness, so that seat's location was ineligible for a
Barricaded Door for the rest of the campaign. Nothing in a `--trace` log flags it; the evidence is
the absent `gameQuestion`.

Verifying needs `--undo`, because the export is captured after the auto-placement: rewind to before
the encounter draw (`--undo 9` there, the step that drew the card) and re-answer the draw, then
compare the pending question — forced placement before the fix, a two-target `ChooseOne` after.

Related: [[project_threat_area_action_is_not_bearer_only]] is the same `placementLocation`
behaviour cutting the other way, where treating a threat-area card as being at the location is the
*correct* reading.
