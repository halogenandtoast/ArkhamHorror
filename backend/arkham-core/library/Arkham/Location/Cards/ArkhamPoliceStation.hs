module Arkham.Location.Cards.ArkhamPoliceStation
  ( arkhamPoliceStation
  , ArkhamPoliceStation(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ArkhamPoliceStation = ArkhamPoliceStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamPoliceStation :: LocationCard ArkhamPoliceStation
arkhamPoliceStation =
  location ArkhamPoliceStation Cards.arkhamPoliceStation 3 (PerPlayer 2)

instance HasAbilities ArkhamPoliceStation where
  getAbilities (ArkhamPoliceStation attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage ArkhamPoliceStation where
  runMessage msg (ArkhamPoliceStation attrs) =
    ArkhamPoliceStation <$> runMessage msg attrs
