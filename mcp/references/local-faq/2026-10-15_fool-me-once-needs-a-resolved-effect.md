---
title: "\"Fool me once...\" needs an effect of the treachery to have actually resolved"
date_added: 2026-10-15
source: rules Q&A relayed by the user (designer answer; venue not recorded — confirm attribution)
affects:
  - "\"Fool me once...\" (06156)"
  - Ward of Protection (01065)
  - Ancient Evils (01166)
  - IgnoreRevelation
  - TreacheryWithResolvedEffectsBy
---

# "Fool me once..." needs an effect of the treachery to have actually resolved

> Q: Hi, I have a question about Fool Me Once and Ward of Protection. If I draw
> a treachery with only a Revelation effect and no other effects or keywords
> (for example Ancient Evils), and I cancel the Revelation effect with Ward of
> Protection, can I play Fool Me Once on that Ancient Evils? Or does the line
> "... after resolving any of its effects" prevent me from playing Fool Me Once
> here, as I canceled the only effect that would've been resolved?
>
> A: No; in order to play "Fool me once..." and attach a treachery, you need to
> have resolved an effect on that treachery.

"Fool me once..." reads "Play when you would discard a treachery card **after
resolving any of its effects**." That clause is a real requirement, not flavor:
if nothing on the treachery resolved, the event cannot be played, even though
the treachery is still being discarded by you and the discard window still
opens.

So cancelling Ancient Evils' Revelation with Ward of Protection leaves nothing
to attach to — Ancient Evils goes to the discard pile and "Fool me once..."
stays in hand.

Related: [2026-06-27_fool-me-once-foreign-signature-weakness.md](2026-06-27_fool-me-once-foreign-signature-weakness.md)
(what happens when the play *is* legal but the treachery is another
investigator's signature weakness).

## Affected cards / systems

- "Fool me once..." (06156) — `backend/arkham-api/library/Arkham/Event/Cards/TheDreamEaters.hs:114` (fast window), `backend/arkham-api/library/Arkham/Event/Events/FoolMeOnce1.hs`
- `TreacheryWithResolvedEffectsBy` — `backend/arkham-api/library/Arkham/Game.hs:1832`, backed by `treacheryResolved` (`Arkham/Treachery/Types.hs:80`), written only by `Arkham/Treachery.hs:34` and `Arkham/Treachery/Runner.hs:143` when a `Revelation` message is dispatched to the treachery.
- Ward of Protection (01065) and every other `cancelRevelation` / `cancelCardEffects` caller — `Arkham/Message/Lifted.hs:2195`.

## Implementation status

- **"Fool me once..." (06156)**: ✅ already matched the ruling — no change.
  Its fast window is `TreacheryWouldBeDiscarded #when (TreacheryWithResolvedEffectsBy You <> TreacheryDiscardedBy You)`.
  `treacheryResolved` is only populated when a `Revelation` message actually
  reaches the treachery entity. Ward of Protection's `#when DrawCard` window
  resolves before `ResolveTreachery`, so by the time `ResolveTreachery` reads
  modifiers the `IgnoreRevelation` branch is taken: it pushes **no**
  `Revelation` at all and discards the treachery directly
  (`Arkham/Game/Runner.hs:4066`). `treacheryResolved` stays empty, the matcher
  fails, and the event is never offered.

- **⚠️ Open edge case (unchanged)**: `treacheryResolved` tracks only Revelation
  dispatch, so a cancelled treachery that nevertheless resolved a *keyword*
  (Surge — `GainSurge` is pushed in `DrewTreachery` regardless of
  `IgnoreRevelation`) also reads as "nothing resolved" and blocks
  "Fool me once...". The question's wording ("no other effects or keywords")
  hints a resolved keyword might qualify, but the answer does not say so.
  Left as-is pending a clarification.
