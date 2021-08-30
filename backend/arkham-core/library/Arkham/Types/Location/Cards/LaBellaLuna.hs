module Arkham.Types.Location.Cards.LaBellaLuna
  ( laBellaLuna
  , LaBellaLuna(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (laBellaLuna)
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

instance HasAbilities env LaBellaLuna where
  getAbilities i w (LaBellaLuna a) =
    withBaseAbilities i w a $ pure [locationResignAction a]

instance LocationRunner env => RunMessage env LaBellaLuna where
  runMessage msg (LaBellaLuna attrs) = LaBellaLuna <$> runMessage msg attrs
