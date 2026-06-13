module Arkham.Location.Cards.FloatingSpire (floatingSpire) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FloatingSpire = FloatingSpire LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

floatingSpire :: LocationCard FloatingSpire
floatingSpire = location FloatingSpire Cards.floatingSpire 4 (Static 1)

-- TODO: abilities

instance RunMessage FloatingSpire where
  runMessage msg (FloatingSpire attrs) = runQueueT $ FloatingSpire <$> liftRunMessage msg attrs
