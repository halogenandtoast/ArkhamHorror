# When implementing a card keep the following in mind

## Does the card access the encounter deck:

```haskell
hasEncounterDeck <- can.target.encounterDeck iid
when hasEncounterDeck $ do
  key <- getEncounterDeckKey iid
  push $ DiscardUntilFirst iid (toSource attrs) (Deck.EncounterDeckByKey key) #enemy
```

## Does the card do healing

```haskell
getHealHorrorMessage :: (HasGame m, Sourceable a) => a -> Int -> InvestigatorId -> m (Maybe Message)
canHaveHorrorHealed :: (HasGame m, Sourceable a) => a -> InvestigatorId -> m (Maybe InvestigatorId)
```

## Does the card affect others

```haskell
affectsOthers :: InvestigatorMatcher -> InvestigatorMatcher
```

## Other
* Make sure to use `withBaseAbilities` or `withRevealedAbilities` when relevant
* Make sure to only use beginSkillTest if non-bold skill test
* Avoid using `You` outside of CardDef and Abilities
