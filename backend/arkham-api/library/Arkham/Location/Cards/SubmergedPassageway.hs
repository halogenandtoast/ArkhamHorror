module Arkham.Location.Cards.SubmergedPassageway (submergedPassageway) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SubmergedPassageway = SubmergedPassageway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

submergedPassageway :: LocationCard SubmergedPassageway
submergedPassageway = locationWith SubmergedPassageway Cards.submergedPassageway 5 (PerPlayer 1) connectsToAdjacent

instance HasAbilities SubmergedPassageway where
  getAbilities (SubmergedPassageway attrs) =
    extendRevealed attrs []

instance RunMessage SubmergedPassageway where
  runMessage msg (SubmergedPassageway attrs) = runQueueT $ case msg of
    _ -> SubmergedPassageway <$> liftRunMessage msg attrs
