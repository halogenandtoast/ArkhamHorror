module Arkham.Location.Cards.NarrowShaft (narrowShaft, NarrowShaft (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Draw.Types
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers
import Arkham.Window (getBatchId)

newtype NarrowShaft = NarrowShaft LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narrowShaft :: LocationCard NarrowShaft
narrowShaft =
  locationWith NarrowShaft Cards.narrowShaft 2 (PerPlayer 1)
    $ (connectsToL .~ adjacentLocations)
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasAbilities NarrowShaft where
  getAbilities (NarrowShaft attrs) =
    extendRevealed
      attrs
      [ skillTestAbility
          $ mkAbility attrs 1
          $ forced
          $ WouldMove #when You AnySource (be attrs) UnrevealedLocation
      , restrictedAbility
          attrs
          2
          (oneOf [notExists $ LocationInDirection dir (be attrs) | dir <- [Above, Below, RightOf]])
          $ forced (RevealLocation #when Anyone $ be attrs)
      ]

instance RunMessage NarrowShaft where
  runMessage msg l@(NarrowShaft attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      batchId <- getBatchId . concat <$> getWindowStack
      push $ CancelBatch batchId
      assignDamage iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck 1
      pure l
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      case drewCards.cards of
        [card] -> do
          placeAbove <- placeAtDirection Above attrs >>= \f -> f card
          placeRight <- placeAtDirection RightOf attrs >>= \f -> f card
          aboveEmpty <- directionEmpty attrs Above
          rightEmpty <- directionEmpty attrs RightOf
          chooseOrRunOneM iid do
            when aboveEmpty $ labeled "Place Above" $ pushAll placeAbove
            when rightEmpty $ labeled "Place to the Right" $ pushAll placeRight
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    _ -> NarrowShaft <$> liftRunMessage msg attrs
