module Arkham.Location.Cards.CafeLunaCoterieHaunt (cafeLunaCoterieHaunt) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CafeLunaCoterieHaunt = CafeLunaCoterieHaunt LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cafeLunaCoterieHaunt :: LocationCard CafeLunaCoterieHaunt
cafeLunaCoterieHaunt = symbolLabel $ location CafeLunaCoterieHaunt Cards.cafeLunaCoterieHaunt 0 (Static 0)

instance HasAbilities CafeLunaCoterieHaunt where
  getAbilities (CafeLunaCoterieHaunt attrs) =
    extendRevealed attrs []

instance RunMessage CafeLunaCoterieHaunt where
  runMessage msg (CafeLunaCoterieHaunt attrs) = runQueueT $ case msg of
    _ -> CafeLunaCoterieHaunt <$> liftRunMessage msg attrs
