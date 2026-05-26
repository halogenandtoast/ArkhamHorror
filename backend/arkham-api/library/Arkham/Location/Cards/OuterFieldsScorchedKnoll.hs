module Arkham.Location.Cards.OuterFieldsScorchedKnoll (outerFieldsScorchedKnoll) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OuterFieldsScorchedKnoll = OuterFieldsScorchedKnoll LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outerFieldsScorchedKnoll :: LocationCard OuterFieldsScorchedKnoll
outerFieldsScorchedKnoll = symbolLabel $ locationWith OuterFieldsScorchedKnoll Cards.outerFieldsScorchedKnoll 0 (Static 0) connectsToAdjacent

instance HasAbilities OuterFieldsScorchedKnoll where
  getAbilities (OuterFieldsScorchedKnoll a) =
    extendRevealed a []

instance RunMessage OuterFieldsScorchedKnoll where
  runMessage msg (OuterFieldsScorchedKnoll attrs) = runQueueT $ case msg of
    _ -> OuterFieldsScorchedKnoll <$> liftRunMessage msg attrs
