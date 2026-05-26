module Arkham.Location.Cards.OuterFieldsDesolateHills (outerFieldsDesolateHills) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OuterFieldsDesolateHills = OuterFieldsDesolateHills LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outerFieldsDesolateHills :: LocationCard OuterFieldsDesolateHills
outerFieldsDesolateHills = symbolLabel $ locationWith OuterFieldsDesolateHills Cards.outerFieldsDesolateHills 0 (Static 0) connectsToAdjacent

instance HasAbilities OuterFieldsDesolateHills where
  getAbilities (OuterFieldsDesolateHills a) =
    extendRevealed a []

instance RunMessage OuterFieldsDesolateHills where
  runMessage msg (OuterFieldsDesolateHills attrs) = runQueueT $ case msg of
    _ -> OuterFieldsDesolateHills <$> liftRunMessage msg attrs
