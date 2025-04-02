module Arkham.Location.Cards.SculpturedCrypt (sculpturedCrypt) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SculpturedCrypt = SculpturedCrypt LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sculpturedCrypt :: LocationCard SculpturedCrypt
sculpturedCrypt = location SculpturedCrypt Cards.sculpturedCrypt 4 (Static 0)

instance HasAbilities SculpturedCrypt where
  getAbilities (SculpturedCrypt attrs) =
    extendRevealed attrs []

instance RunMessage SculpturedCrypt where
  runMessage msg (SculpturedCrypt attrs) = runQueueT $ case msg of
    _ -> SculpturedCrypt <$> liftRunMessage msg attrs
