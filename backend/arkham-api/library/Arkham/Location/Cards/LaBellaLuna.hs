module Arkham.Location.Cards.LaBellaLuna (laBellaLuna) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (laBellaLuna)
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheHouseAlwaysWins.Helpers

newtype LaBellaLuna = LaBellaLuna LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laBellaLuna :: LocationCard LaBellaLuna
laBellaLuna = symbolLabel $ location LaBellaLuna Cards.laBellaLuna 2 (PerPlayer 1)

instance HasAbilities LaBellaLuna where
  getAbilities (LaBellaLuna a) = extendRevealed1 a $ scenarioI18n $ withI18nTooltip "laBellaLuna.resign" $ locationResignAction a

instance RunMessage LaBellaLuna where
  runMessage msg (LaBellaLuna attrs) = LaBellaLuna <$> runMessage msg attrs
