module Arkham.Types.Location.Cards.LaBellaLuna
  ( laBellaLuna
  , LaBellaLuna(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (laBellaLuna)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol

newtype LaBellaLuna = LaBellaLuna LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laBellaLuna :: LocationCard LaBellaLuna
laBellaLuna =
  location LaBellaLuna Cards.laBellaLuna 2 (PerPlayer 1) Moon [Circle]

instance HasModifiersFor env LaBellaLuna

instance HasAbilities env LaBellaLuna where
  getAbilities _ _ (LaBellaLuna a) = pure [locationResignAction a]

instance LocationRunner env => RunMessage env LaBellaLuna where
  runMessage msg (LaBellaLuna attrs) = LaBellaLuna <$> runMessage msg attrs
