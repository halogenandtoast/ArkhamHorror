module Arkham.Location.Cards.CasinoFloorBusyNight (casinoFloorBusyNight) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.FortuneAndFolly.Helpers

newtype CasinoFloorBusyNight = CasinoFloorBusyNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casinoFloorBusyNight :: LocationCard CasinoFloorBusyNight
casinoFloorBusyNight = symbolLabel $ location CasinoFloorBusyNight Cards.casinoFloorBusyNight 0 (Static 0)

instance HasAbilities CasinoFloorBusyNight where
  getAbilities (CasinoFloorBusyNight a) =
    extendRevealed
      a
      [scenarioI18n $ withI18nTooltip "casinoFloorBusyNight.resign" $ locationResignAction a]

instance RunMessage CasinoFloorBusyNight where
  runMessage msg (CasinoFloorBusyNight attrs) = runQueueT $ case msg of
    _ -> CasinoFloorBusyNight <$> liftRunMessage msg attrs
