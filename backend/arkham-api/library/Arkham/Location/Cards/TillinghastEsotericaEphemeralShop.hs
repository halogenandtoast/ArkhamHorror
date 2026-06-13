module Arkham.Location.Cards.TillinghastEsotericaEphemeralShop (tillinghastEsotericaEphemeralShop) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TillinghastEsotericaEphemeralShop = TillinghastEsotericaEphemeralShop LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tillinghastEsotericaEphemeralShop :: LocationCard TillinghastEsotericaEphemeralShop
tillinghastEsotericaEphemeralShop = location TillinghastEsotericaEphemeralShop Cards.tillinghastEsotericaEphemeralShop 4 (Static 3)

-- TODO: abilities

instance RunMessage TillinghastEsotericaEphemeralShop where
  runMessage msg (TillinghastEsotericaEphemeralShop attrs) = runQueueT $ TillinghastEsotericaEphemeralShop <$> liftRunMessage msg attrs
