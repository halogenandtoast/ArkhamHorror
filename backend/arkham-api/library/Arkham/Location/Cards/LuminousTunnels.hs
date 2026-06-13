module Arkham.Location.Cards.LuminousTunnels (luminousTunnels) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LuminousTunnels = LuminousTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luminousTunnels :: LocationCard LuminousTunnels
luminousTunnels = location LuminousTunnels Cards.luminousTunnels 2 (Static 2)

-- TODO: abilities

instance RunMessage LuminousTunnels where
  runMessage msg (LuminousTunnels attrs) = runQueueT $ LuminousTunnels <$> liftRunMessage msg attrs
