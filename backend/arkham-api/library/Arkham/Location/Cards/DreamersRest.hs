module Arkham.Location.Cards.DreamersRest (dreamersRest) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DreamersRest = DreamersRest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamersRest :: LocationCard DreamersRest
dreamersRest = location DreamersRest Cards.dreamersRest 1 (Static 5)

-- TODO: abilities

instance RunMessage DreamersRest where
  runMessage msg (DreamersRest attrs) = runQueueT $ DreamersRest <$> liftRunMessage msg attrs
