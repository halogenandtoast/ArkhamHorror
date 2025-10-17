module Arkham.Location.Cards.AvenidaDeMayo (avenidaDeMayo) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AvenidaDeMayo = AvenidaDeMayo LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

avenidaDeMayo :: LocationCard AvenidaDeMayo
avenidaDeMayo = setLabel "a" $ location AvenidaDeMayo Cards.avenidaDeMayo 0 (PerPlayer 1)

instance HasAbilities AvenidaDeMayo where
  getAbilities (AvenidaDeMayo attrs) =
    extendRevealed attrs []

instance RunMessage AvenidaDeMayo where
  runMessage msg (AvenidaDeMayo attrs) = runQueueT $ case msg of
    _ -> AvenidaDeMayo <$> liftRunMessage msg attrs
