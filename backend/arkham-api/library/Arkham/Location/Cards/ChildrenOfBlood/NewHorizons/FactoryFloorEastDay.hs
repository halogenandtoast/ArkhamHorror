module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.FactoryFloorEastDay (factoryFloorEastDay) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype FactoryFloorEastDay = FactoryFloorEastDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

factoryFloorEastDay :: LocationCard FactoryFloorEastDay
factoryFloorEastDay = symbolLabel $ location FactoryFloorEastDay Cards.factoryFloorEastDay 2 (PerPlayer 1)

instance HasAbilities FactoryFloorEastDay where
  getAbilities (FactoryFloorEastDay a) = extendRevealed a []

instance RunMessage FactoryFloorEastDay where
  runMessage msg (FactoryFloorEastDay attrs) = runQueueT $ FactoryFloorEastDay <$> liftRunMessage msg attrs
