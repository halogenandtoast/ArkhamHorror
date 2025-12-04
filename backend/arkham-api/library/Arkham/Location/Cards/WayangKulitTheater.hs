module Arkham.Location.Cards.WayangKulitTheater (wayangKulitTheater) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WayangKulitTheater = WayangKulitTheater LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wayangKulitTheater :: LocationCard WayangKulitTheater
wayangKulitTheater = symbolLabel $ location WayangKulitTheater Cards.wayangKulitTheater 0 (Static 0)

instance HasAbilities WayangKulitTheater where
  getAbilities (WayangKulitTheater a) =
    extendRevealed a []

instance RunMessage WayangKulitTheater where
  runMessage msg (WayangKulitTheater attrs) = runQueueT $ case msg of
    _ -> WayangKulitTheater <$> liftRunMessage msg attrs
