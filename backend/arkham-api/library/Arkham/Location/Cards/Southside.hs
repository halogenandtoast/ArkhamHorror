module Arkham.Location.Cards.Southside (southside) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Southside = Southside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

southside :: LocationCard Southside
southside = location Southside Cards.southside 3 (Static 1)

-- TODO: abilities

instance RunMessage Southside where
  runMessage msg (Southside attrs) = runQueueT $ Southside <$> liftRunMessage msg attrs
