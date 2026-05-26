module Arkham.Location.Cards.OuterFieldsBlightedCornfields (outerFieldsBlightedCornfields) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OuterFieldsBlightedCornfields = OuterFieldsBlightedCornfields LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outerFieldsBlightedCornfields :: LocationCard OuterFieldsBlightedCornfields
outerFieldsBlightedCornfields = symbolLabel $ locationWith OuterFieldsBlightedCornfields Cards.outerFieldsBlightedCornfields 0 (Static 0) connectsToAdjacent

instance HasAbilities OuterFieldsBlightedCornfields where
  getAbilities (OuterFieldsBlightedCornfields a) =
    extendRevealed a []

instance RunMessage OuterFieldsBlightedCornfields where
  runMessage msg (OuterFieldsBlightedCornfields attrs) = runQueueT $ case msg of
    _ -> OuterFieldsBlightedCornfields <$> liftRunMessage msg attrs
