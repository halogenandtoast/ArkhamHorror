module Arkham.Location.Cards.DrKenslersOffice (drKenslersOffice) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DrKenslersOffice = DrKenslersOffice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drKenslersOffice :: LocationCard DrKenslersOffice
drKenslersOffice = location DrKenslersOffice Cards.drKenslersOffice 3 (PerPlayer 2)

instance HasAbilities DrKenslersOffice where
  getAbilities (DrKenslersOffice attrs) =
    extendRevealed attrs []

instance RunMessage DrKenslersOffice where
  runMessage msg (DrKenslersOffice attrs) = runQueueT $ case msg of
    _ -> DrKenslersOffice <$> liftRunMessage msg attrs
