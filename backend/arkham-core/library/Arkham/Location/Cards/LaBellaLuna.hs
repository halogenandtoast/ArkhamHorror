module Arkham.Location.Cards.LaBellaLuna (
  laBellaLuna,
  LaBellaLuna (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (laBellaLuna)
import Arkham.Location.Runner

newtype LaBellaLuna = LaBellaLuna LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

laBellaLuna :: LocationCard LaBellaLuna
laBellaLuna = location LaBellaLuna Cards.laBellaLuna 2 (PerPlayer 1)

instance HasAbilities LaBellaLuna where
  getAbilities (LaBellaLuna a) = withBaseAbilities a [locationResignAction a]

instance RunMessage LaBellaLuna where
  runMessage msg (LaBellaLuna attrs) = LaBellaLuna <$> runMessage msg attrs
