module Arkham.Types.Location.Cards.LaBellaLuna
  ( laBellaLuna
  , LaBellaLuna(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Name
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype LaBellaLuna = LaBellaLuna LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laBellaLuna :: LaBellaLuna
laBellaLuna = LaBellaLuna $ baseAttrs
  "02070"
  (Name "La Bella Luna" Nothing)
  EncounterSet.TheHouseAlwaysWins
  2
  (PerPlayer 1)
  Moon
  [Circle]
  [Arkham]

instance HasModifiersFor env LaBellaLuna where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env LaBellaLuna where
  getActions = withResignAction

instance LocationRunner env => RunMessage env LaBellaLuna where
  runMessage msg (LaBellaLuna attrs) = LaBellaLuna <$> runMessage msg attrs
