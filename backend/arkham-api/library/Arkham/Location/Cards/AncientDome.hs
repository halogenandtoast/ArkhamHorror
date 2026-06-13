module Arkham.Location.Cards.AncientDome (ancientDome) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AncientDome = AncientDome LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientDome :: LocationCard AncientDome
ancientDome = location AncientDome Cards.ancientDome 2 (Static 0)

-- TODO: abilities

instance RunMessage AncientDome where
  runMessage msg (AncientDome attrs) = runQueueT $ AncientDome <$> liftRunMessage msg attrs
