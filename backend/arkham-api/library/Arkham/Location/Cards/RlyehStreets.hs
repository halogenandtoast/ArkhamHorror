module Arkham.Location.Cards.RlyehStreets (rlyehStreets) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RlyehStreets = RlyehStreets LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rlyehStreets :: LocationCard RlyehStreets
rlyehStreets = location RlyehStreets Cards.rlyehStreets 2 (Static 3)

-- TODO: abilities

instance RunMessage RlyehStreets where
  runMessage msg (RlyehStreets attrs) = runQueueT $ RlyehStreets <$> liftRunMessage msg attrs
