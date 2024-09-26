module Arkham.Location.Cards.GilmanHouseInTooDeep (
  gilmanHouseInTooDeep,
  GilmanHouseInTooDeep (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype GilmanHouseInTooDeep = GilmanHouseInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gilmanHouseInTooDeep :: LocationCard GilmanHouseInTooDeep
gilmanHouseInTooDeep = locationWith GilmanHouseInTooDeep Cards.gilmanHouseInTooDeep 3 (PerPlayer 2) connectsToAdjacent

instance HasAbilities GilmanHouseInTooDeep where
  getAbilities (GilmanHouseInTooDeep attrs) =
    extendRevealed attrs []

instance RunMessage GilmanHouseInTooDeep where
  runMessage msg (GilmanHouseInTooDeep attrs) = runQueueT $ case msg of
    _ -> GilmanHouseInTooDeep <$> liftRunMessage msg attrs
