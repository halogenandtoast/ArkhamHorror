---
title: project-addcampaigncardtodeck-reuses-the-set-aside-copy
description: "Looping addCampaignCardToDeck over investigators with one CardDef hands them all the SAME card id, but only when the card is set aside — fetchCard prefers a set-aside copy and nothing consumes it"
---

```haskell
for_ investigators \iid -> addCampaignCardToDeck iid ShuffleIn Treacheries.someWeakness
```

gives every investigator **the same card id** — *if that card is in the set-aside pool*.

`addCampaignCardToDeck` resolves its argument through `fetchCard`, and
`instance FetchCard CardDef` (`Helpers/FetchCard.hs:54-58`) is

```haskell
maybe (Just <$> genCard def) (pure . Just) =<< maybeGetSetAsideCard def
```

so a set-aside copy wins over minting a new one. But `maybeGetSetAsideCard`
(`Helpers/Query.hs:131-135`) only *matches* — `selectMaybeT $ SetAsideCardMatch $ cardIs def` —
and the `AddCampaignCardToDeck` handler (`Scenario/Runner.hs:950-956`) does
`replaceCard card.id card'` and records the owner, never removing the card from
`scenarioSetAsideCards`. Nothing in the loop consumes the pool, so every iteration fetches the
same copy and the last owner wins.

**The idiom is correct when the card is NOT set aside** — then `genCard def` mints a fresh card
per call. That is why the same shape is fine in plenty of scenarios and wrong in the one that
also did `setAsideEvery $ cardIs def` because its guide said to set the copies aside.

Fix at the call site: zip the investigators against **distinct** set-aside copies.

```haskell
copies <- select $ SetAsideCardMatch $ cardIs Treacheries.someWeakness
for_ (zip investigators copies) \(iid, card) ->
  addCampaignCardToDeck iid ShuffleIn card
```

Leave a comment saying why the plain `for_` is wrong, or it gets "simplified" back.

Do **not** fix this by making the handler consume the set-aside pile: several official scenarios
hand the *same* set-aside card to a chosen investigator and then read it back out of the pool.

Found in Ages Unwound's Scenario IV (Unstuck), whose Investigator Defeat gives each defeated
seat a Time Runs Backwards weakness while setup had set all four copies aside. The symptom is
invisible at compile time and only shows when two investigators are defeated in the same game.
