module Arkham.Location.Cards.OuterFieldsRancidCrops (outerFieldsRancidCrops) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OuterFieldsRancidCrops = OuterFieldsRancidCrops LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outerFieldsRancidCrops :: LocationCard OuterFieldsRancidCrops
outerFieldsRancidCrops = symbolLabel $ locationWith OuterFieldsRancidCrops Cards.outerFieldsRancidCrops 0 (Static 0) connectsToAdjacent

instance HasAbilities OuterFieldsRancidCrops where
  getAbilities (OuterFieldsRancidCrops a) =
    extendRevealed a []

instance RunMessage OuterFieldsRancidCrops where
  runMessage msg (OuterFieldsRancidCrops attrs) = runQueueT $ case msg of
    _ -> OuterFieldsRancidCrops <$> liftRunMessage msg attrs
