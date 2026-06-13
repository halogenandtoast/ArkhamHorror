module Arkham.Location.Cards.ChurningChasm (churningChasm) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ChurningChasm = ChurningChasm LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

churningChasm :: LocationCard ChurningChasm
churningChasm = location ChurningChasm Cards.churningChasm 2 (Static 1)

-- TODO: abilities

instance RunMessage ChurningChasm where
  runMessage msg (ChurningChasm attrs) = runQueueT $ ChurningChasm <$> liftRunMessage msg attrs
