module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.FactoryFloorWestDay (factoryFloorWestDay) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype FactoryFloorWestDay = FactoryFloorWestDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

factoryFloorWestDay :: LocationCard FactoryFloorWestDay
factoryFloorWestDay = symbolLabel $ location FactoryFloorWestDay Cards.factoryFloorWestDay 2 (PerPlayer 1)

instance HasAbilities FactoryFloorWestDay where
  getAbilities (FactoryFloorWestDay a) = extendRevealed a []

instance RunMessage FactoryFloorWestDay where
  runMessage msg (FactoryFloorWestDay attrs) = runQueueT $ FactoryFloorWestDay <$> liftRunMessage msg attrs
