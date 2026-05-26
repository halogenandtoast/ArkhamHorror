module Arkham.Location.Cards.OuterFieldsBloodiedPaths (outerFieldsBloodiedPaths) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OuterFieldsBloodiedPaths = OuterFieldsBloodiedPaths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outerFieldsBloodiedPaths :: LocationCard OuterFieldsBloodiedPaths
outerFieldsBloodiedPaths = symbolLabel $ locationWith OuterFieldsBloodiedPaths Cards.outerFieldsBloodiedPaths 0 (Static 0) connectsToAdjacent

instance HasAbilities OuterFieldsBloodiedPaths where
  getAbilities (OuterFieldsBloodiedPaths a) =
    extendRevealed a []

instance RunMessage OuterFieldsBloodiedPaths where
  runMessage msg (OuterFieldsBloodiedPaths attrs) = runQueueT $ case msg of
    _ -> OuterFieldsBloodiedPaths <$> liftRunMessage msg attrs
