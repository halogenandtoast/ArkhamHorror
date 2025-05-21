module Arkham.Location.Cards.NarrowShaft (narrowShaft) where

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
    $ connectsToAdjacent
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasAbilities NarrowShaft where
  getAbilities (NarrowShaft a) =
    extendRevealed
      a
      [ skillTestAbility $ mkAbility a 1 $ forced $ WouldMove #when You AnySource (be a) UnrevealedLocation
      , restricted a 2 (oneOf [notExists $ LocationInDirection dir (be a) | dir <- [Above, Below, RightOf]])
          $ forced (RevealLocation #when Anyone $ be a)
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
          aboveEmpty <- directionEmpty attrs Above
          rightEmpty <- directionEmpty attrs RightOf
          chooseOrRunOneM iid $ scenarioI18n do
            when aboveEmpty $ labeled' "above" $ placeAtDirection_ Above attrs card
            when rightEmpty $ labeled' "right" $ placeAtDirection_ RightOf attrs card
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    _ -> NarrowShaft <$> liftRunMessage msg attrs
