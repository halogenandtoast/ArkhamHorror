---
title: "\"Limit 1 per location\" caps the copies in play at a location, not where you may play from"
date_added: 2026-11-21
source: Arkham Grimoire v1.0 (2026), glossary "Limits and Maximums"
affects:
  - Light of Ilarnek (14026)
  - cdLimits / LimitPerLocation
---

# "Limit 1 per location" caps the copies in play at a location, not where you may play from

The Grimoire glossary settles this:

> "Limit X per [card/game element]" is a limit that appears on some cards, and restricts the
> number of copies of that card (by title) that can be in play at the same time.

So "Limit 1 per location" restricts **attachments**: one copy by title per location. It is
not a statement about the location you are standing in when you play the card.

## Why it matters

`cdLimits = [LimitPerLocation n]` is enforced by `passesPerLocationLimits`
(`Arkham/Helpers/Card.hs`), which tests the **investigator's own location** at play time.
That is right for the usual case — a trap that attaches to your own location — and wrong for
a card that attaches somewhere else.

**Light of Ilarnek (14026)** attaches to *any revealed location*. With a `cdLimits` entry,
standing on a location that already held a copy made a second copy unplayable, even though
it could legally have been attached elsewhere. The def therefore carries **no** `cdLimits`,
and the card filters the attach choice instead:

```haskell
RevealedLocation
  <> LocationCanHaveAttachments
  <> not_ (LocationWithAttachedEvent $ eventIs Cards.lightOfIlarnek3)
```

## Rule of thumb

- attaches to **your** location ⇒ `cdLimits = [LimitPerLocation n]` is correct and enough.
- attaches to a **chosen** location ⇒ enforce it at the choice, and leave `cdLimits` off, or
  the play is blocked by a location the card was never going to attach to.
