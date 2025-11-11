module Arkham.Location.Cards.OwnersOffice (ownersOffice) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OwnersOffice = OwnersOffice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ownersOffice :: LocationCard OwnersOffice
ownersOffice = symbolLabel $ location OwnersOffice Cards.ownersOffice 0 (Static 0)

instance HasAbilities OwnersOffice where
  getAbilities (OwnersOffice attrs) =
    extendRevealed attrs []

instance RunMessage OwnersOffice where
  runMessage msg (OwnersOffice attrs) = runQueueT $ case msg of
    _ -> OwnersOffice <$> liftRunMessage msg attrs
