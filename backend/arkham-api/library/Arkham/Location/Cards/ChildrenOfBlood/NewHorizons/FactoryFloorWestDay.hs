module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.FactoryFloorWestDay (factoryFloorWestDay) where

import Arkham.Ability
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FactoryFloorWestDay = FactoryFloorWestDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

factoryFloorWestDay :: LocationCard FactoryFloorWestDay
factoryFloorWestDay = symbolLabel $ location FactoryFloorWestDay Cards.factoryFloorWestDay 2 (PerPlayer 1)

instance HasAbilities FactoryFloorWestDay where
  getAbilities (FactoryFloorWestDay a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage FactoryFloorWestDay where
  runMessage msg l@(FactoryFloorWestDay attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs) \iid -> loseResources iid (attrs.ability 1) 1
      pure l
    _ -> FactoryFloorWestDay <$> liftRunMessage msg attrs
