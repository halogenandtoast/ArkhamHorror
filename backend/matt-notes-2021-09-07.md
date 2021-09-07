# Parsons Notes

## Lots of `HashMap CardCode CardDef` where these things are known

Eg: we have `arhkham-core/library/Arkham/Treachery/Cards.hs` which has `allPlayerTreacheryCards`.

```haskell
allPlayerTreacheryCards :: HashMap CardCode CardDef
allPlayerTreacheryCards = mapFromList $ map
  (toCardCode &&& id)
  [ abandonedAndAlone
  , acrossSpaceAndTime
  , amnesia
  , angeredSpirits
  , atychiphobia
  , chronophobia
  , coverUp
  , crisisOfIdentity
  , curseOfTheRougarou
  , drawingTheSign
  , finalRhapsody
  , haunted
  , hospitalDebts
  , hypochondria
  , indebted
  , internalInjury
  , overzealous
  , paranoia
  , psychosis
  , rexsCurse
  , searchingForIzzie
  , shellShock
  , smiteTheWicked
  , starsOfHyades
  , wrackedByNightmares
  ]

-- snip

abandonedAndAlone :: CardDef
abandonedAndAlone = (weakness "01015" "Abandoned and Alone")
  { cdCardTraits = setFromList [Madness]
  }
```

And `abandonedAndAlone` has a corresponding type:

```haskell
module Arkham.Types.Treachery.Cards.AbandonedAndAlone where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards (abandonedAndAlone)
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype AbandonedAndAlone = AbandonedAndAlone TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedAndAlone :: TreacheryCard AbandonedAndAlone
abandonedAndAlone = treachery AbandonedAndAlone Cards.abandonedAndAlone

instance TreacheryRunner env => RunMessage env AbandonedAndAlone where
  runMessage msg t@(AbandonedAndAlone attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ pushAll
        [InvestigatorDirectDamage iid source 0 2, RemoveDiscardFromGame iid]
    _ -> AbandonedAndAlone <$> runMessage msg attrs
```

Two questions:

1. How is this lookup map used?
2. What's the `treachery` constructor?

### how is map used?

It is smashed into `allEncounterTreacheryCards` to yield `allTreacheryCards`.
It is used directly in `validate/Main.hs` (but that could be replaced with `allTreacheryCards`).
And it is mashed into `allPlayerCards` with other things.

`allTreacheryCards` has one use site:

```haskell
instance HasCardDef TreacheryAttrs where
  toCardDef a = case lookup (treacheryCardCode a) allTreacheryCards of
    Just def -> def
    Nothing ->
      error $ "missing card def for treachery " <> show (treacheryCardCode a)
```

This is a function `a -> CardDef`, or specializing, `TreacheryAttrs -> CardDef`.

So - we construct a static `HashMap CardCode CardDef`.
Then, in a type class instance, we use that to look up the `CardDef`.

- `CardDef` has a `cdCardCode :: CardCode` field.
- `TreacheryAttrs`

### What is treachery constructor?

```haskell
baseTreachery
  :: CardCode -> Name -> Maybe (EncounterSet, Int) -> Bool -> CardDef
baseTreachery cardCode name mEncounterSet isWeakness = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = if isWeakness then PlayerTreacheryType else TreacheryType
  , cdWeakness = isWeakness
  , cdClassSymbol = if isWeakness then Just Neutral else Nothing
  , cdSkills = mempty
  , cdCardTraits = mempty
  , cdKeywords = mempty
  , cdFastWindow = Nothing
  , cdAction = Nothing
  , cdRevelation = True
  , cdVictoryPoints = Nothing
  , cdCriteria = mempty
  , cdCommitRestrictions = mempty
  , cdAttackOfOpportunityModifiers = mempty
  , cdPermanent = False
  , cdEncounterSet = fst <$> mEncounterSet
  , cdEncounterSetQuantity = snd <$> mEncounterSet
  , cdUnique = False
  , cdDoubleSided = False
  , cdLimits = []
  , cdExceptional = False
  , cdUses = NoUses
  }

weakness :: CardCode -> Name -> CardDef
weakness cardCode name = baseTreachery cardCode name Nothing True

treachery :: CardCode -> Name -> EncounterSet -> Int -> CardDef
treachery cardCode name encounterSet encounterSetQuantity =
  baseTreachery cardCode name (Just (encounterSet, encounterSetQuantity)) False
```

So we construct a `CardDef` - What's a `TreacheryCard` then?

```haskell
module Arkham.Types.Treachery.Attrs where

    type TreacheryCard a = CardBuilder (InvestigatorId, TreacheryId) a

module Arkham.Types.Card

    data CardBuilder ident a = CardBuilder
      { cbCardCode :: CardCode
      , cbCardBuilder :: ident -> a
      }
```

So a `TreacheryCard AbandonedAndAlone` is a `CardBuilder (InvestigatorId, TreacheryId) AbandonedAndAlone`.

# Static vs Dynamic

OK this is starting to click, I think.

All of the cards are static.
They don't appear to change.
The dynamic behavior is all happening in `env` - the environment.

So,

- Type `AbandonedAndAlone` which has Behavior (as specified in `RunMessages` instance)
- Data constructor `AbandonedAndAlone TreacheryAttrs`
- `Arkham.Treachery.Cards.abandonedAndAlone :: CardDef`
- `Arkham.Types.Treacher.Cards.AbandonedAndAlone.abandonedAndAlone :: TreacheryCard AbandonedAndAlone`
    - `TreacheryCard AbandonedAndAlone ~ CardBuilder (InvestigatorId, TreacheryId)`
    - `CardBuilder (InvestigatorId, TreacheryId) ~ (CardCode, (InvestigatorId, TreacheryId) -> AbandonedAndAlone)`

All of this is static.
Which means we can replace it all with *static methods on a type*, or, 

```haskell
class StaticMethod a where
    staticMethod :: proxy a -> Result
```

Inverting our dependencies, we could write:

```haskell
class KnownSymbol sym => HasCardCode ty sym | ty -> sym, sym -> ty

-- injectivity annotation allows us toknow the type, if we know the symbol.
-- could use this form too
type HasCardCode (ty :: Type) = (r :: Symbol) | r -> ty

getCardCodeForType :: forall ty sym. HasCardCode ty sym => proxy ty -> Text
getCardCodeForType _ = symbolVal (Proxy @sym)

getTypeForCardCode :: forall sym ty. HasCardCode ty sym => proxy sym -> Proxy ty
getTypeForCardCode _ = Proxy

class HasTreacheryAttrs a where
    getTreacheryAttrs :: proxy a -> TreacheryAttrs

data AbandonedAndAlone = AbandonedAndAlone

instance HasCardCode AbandonedAndAlone "01015"

instance HasTreacheryAttrs AbandonedAndAlone where
    getTreacheryAttrs _ = 
        ...

instance HasCardDef AbandonedAndAlone where
    getCardDef _ = 
        (weakness "Abandoned And Alone")
            { cdCardTraints = setFromList [Madness]
            }

instance TreacheryRunner env => RunMessage env AbandonedAndAlone where
  runMessage msg AbandonedAndAlone = 
    case msg of
      Revelation iid source | isSource attrs source -> do
        t <$ pushAll
          [InvestigatorDirectDamage iid source 0 2, RemoveDiscardFromGame iid]
      _ -> 
        AbandonedAndAlone <$ runMessage msg attrs
    where
      attrs = 
        getTreacheryAttrs (Proxy @AbandonedAndAlone)
```

Except, uh oh, `runMessage` may very well alter the return.
And, it does.

```haskell
instance TreacheryRunner env => RunMessage env TreacheryAttrs where
  runMessage msg a@TreacheryAttrs {..} = 
    case msg of
      InvestigatorEliminated iid
        | InvestigatorTarget iid `elem` treacheryAttachedTarget -> a
        <$ push (Discard $ toTarget a)
      AttachTreachery tid target | tid == treacheryId ->
        pure $ a & attachedTargetL ?~ target
      PlaceResources target n | isTarget a target -> do
        let amount = fromMaybe 0 treacheryResources + n
        pure $ a & resourcesL ?~ amount
      PlaceEnemyInVoid eid | EnemyTarget eid `elem` treacheryAttachedTarget ->
        a <$ push (Discard $ toTarget a)
      AddTreacheryToHand iid tid | tid == treacheryId ->
        pure $ a & inHandOfL ?~ iid
      Discarded target _ | target `elem` treacheryAttachedTarget ->
        a <$ push (Discard $ toTarget a)
      After (Revelation _ source) | isSource a source -> a <$ when
        (isNothing treacheryAttachedTarget && isNothing treacheryInHandOf)
        (push $ Discard $ toTarget a)
      _ -> pure a
```

So, we have `attachedTargetL`, `resourcesL`, `inHandOfL` - these are all fields of `TreacheryAttrs` that may be modified by our card.

I'm wondering another thing here.

To what extent are we sending messages to cards that they don't have any response to?
The method signature says:

"I am sending a `Message` to this thing, and I'll modify the `env` in some context `m` and return the thing, possibly modified."

But it seems like the pattern is to do a `_ -> pure a` pattern - "if I don't respond to this message, just skip over and return myself."

Well, let's not let scope creep harm the investigation.

# We have a comingling of static and dynamic data

This suggests a change:

```haskell
data StaticTreacheryAttrs = ...

data DynamicTreacheryAttrs = ...

newtype AbandonedAndAlone = AbandonedAndAlone DynamicTreacheryAttrs

instance StaticStuff AbandonedAndAlone ...
```

What other changes flow from this?
