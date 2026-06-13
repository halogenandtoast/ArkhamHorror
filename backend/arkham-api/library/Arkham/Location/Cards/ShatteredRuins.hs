module Arkham.Location.Cards.ShatteredRuins (shatteredRuins) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ShatteredRuins = ShatteredRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shatteredRuins :: LocationCard ShatteredRuins
shatteredRuins = location ShatteredRuins Cards.shatteredRuins 0 (Static 2)

-- TODO: abilities

instance RunMessage ShatteredRuins where
  runMessage msg (ShatteredRuins attrs) = runQueueT $ ShatteredRuins <$> liftRunMessage msg attrs
