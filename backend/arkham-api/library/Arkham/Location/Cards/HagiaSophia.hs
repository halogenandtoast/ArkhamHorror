module Arkham.Location.Cards.HagiaSophia (hagiaSophia) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HagiaSophia = HagiaSophia LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hagiaSophia :: LocationCard HagiaSophia
hagiaSophia = symbolLabel $ location HagiaSophia Cards.hagiaSophia 2 (PerPlayer 1)

instance HasAbilities HagiaSophia where
  getAbilities (HagiaSophia attrs) =
    extendRevealed attrs []

instance RunMessage HagiaSophia where
  runMessage msg (HagiaSophia attrs) = runQueueT $ case msg of
    _ -> HagiaSophia <$> liftRunMessage msg attrs
