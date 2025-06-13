module Arkham.Location.Cards.ReturnToTempleRuins (returnToTempleRuins) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToTempleRuins = ReturnToTempleRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToTempleRuins :: LocationCard ReturnToTempleRuins
returnToTempleRuins = symbolLabel $ location ReturnToTempleRuins Cards.returnToTempleRuins 5 (Static 0)

instance HasAbilities ReturnToTempleRuins where
  getAbilities (ReturnToTempleRuins attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToTempleRuins where
  runMessage msg (ReturnToTempleRuins attrs) = runQueueT $ case msg of
    _ -> ReturnToTempleRuins <$> liftRunMessage msg attrs
