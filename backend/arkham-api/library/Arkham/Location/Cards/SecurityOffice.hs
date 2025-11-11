module Arkham.Location.Cards.SecurityOffice (securityOffice) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SecurityOffice = SecurityOffice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

securityOffice :: LocationCard SecurityOffice
securityOffice = symbolLabel $ location SecurityOffice Cards.securityOffice 0 (Static 0)

instance HasAbilities SecurityOffice where
  getAbilities (SecurityOffice attrs) =
    extendRevealed attrs []

instance RunMessage SecurityOffice where
  runMessage msg (SecurityOffice attrs) = runQueueT $ case msg of
    _ -> SecurityOffice <$> liftRunMessage msg attrs
