module Arkham.Types.Location.Cards.LaBellaLuna
  ( laBellaLuna
  , LaBellaLuna(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (laBellaLuna)
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype LaBellaLuna = LaBellaLuna LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laBellaLuna :: LocationCard LaBellaLuna
laBellaLuna =
  location LaBellaLuna Cards.laBellaLuna 2 (PerPlayer 1) Moon [Circle]

instance HasAbilities LaBellaLuna where
  getAbilities (LaBellaLuna a) = withBaseAbilities a [locationResignAction a]

instance LocationRunner env => RunMessage env LaBellaLuna where
  runMessage msg (LaBellaLuna attrs) = LaBellaLuna <$> runMessage msg attrs
