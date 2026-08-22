module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.FactoryFloorEastNight (factoryFloorEastNight) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype FactoryFloorEastNight = FactoryFloorEastNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

factoryFloorEastNight :: LocationCard FactoryFloorEastNight
factoryFloorEastNight = symbolLabel $ location FactoryFloorEastNight Cards.factoryFloorEastNight 2 (PerPlayer 1)

instance HasAbilities FactoryFloorEastNight where
  getAbilities (FactoryFloorEastNight a) = extendRevealed a []

instance RunMessage FactoryFloorEastNight where
  runMessage msg (FactoryFloorEastNight attrs) = runQueueT $ FactoryFloorEastNight <$> liftRunMessage msg attrs
