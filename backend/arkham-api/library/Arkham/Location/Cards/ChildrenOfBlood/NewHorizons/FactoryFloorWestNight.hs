module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.FactoryFloorWestNight (factoryFloorWestNight) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype FactoryFloorWestNight = FactoryFloorWestNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

factoryFloorWestNight :: LocationCard FactoryFloorWestNight
factoryFloorWestNight = symbolLabel $ location FactoryFloorWestNight Cards.factoryFloorWestNight 2 (PerPlayer 1)

instance HasAbilities FactoryFloorWestNight where
  getAbilities (FactoryFloorWestNight a) = extendRevealed a []

instance RunMessage FactoryFloorWestNight where
  runMessage msg (FactoryFloorWestNight attrs) = runQueueT $ FactoryFloorWestNight <$> liftRunMessage msg attrs
