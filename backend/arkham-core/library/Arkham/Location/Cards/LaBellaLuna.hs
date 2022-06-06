module Arkham.Location.Cards.LaBellaLuna
  ( laBellaLuna
  , LaBellaLuna(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (laBellaLuna)
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Runner

newtype LaBellaLuna = LaBellaLuna LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laBellaLuna :: LocationCard LaBellaLuna
laBellaLuna =
  location LaBellaLuna Cards.laBellaLuna 2 (PerPlayer 1) Moon [Circle]

instance HasAbilities LaBellaLuna where
  getAbilities (LaBellaLuna a) = withBaseAbilities a [locationResignAction a]

instance LocationRunner env => RunMessage LaBellaLuna where
  runMessage msg (LaBellaLuna attrs) = LaBellaLuna <$> runMessage msg attrs
