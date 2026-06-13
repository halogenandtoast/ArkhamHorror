module Arkham.Location.Cards.LostCampsite (lostCampsite) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LostCampsite = LostCampsite LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostCampsite :: LocationCard LostCampsite
lostCampsite = location LostCampsite Cards.lostCampsite 4 (Static 1)

-- TODO: abilities

instance RunMessage LostCampsite where
  runMessage msg (LostCampsite attrs) = runQueueT $ LostCampsite <$> liftRunMessage msg attrs
