module Arkham.Types.Location.Cards.TearThroughSpace
  ( tearThroughSpace
  , TearThroughSpace(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Name
import Arkham.Types.Trait

newtype TearThroughSpace = TearThroughSpace LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tearThroughSpace :: LocationId -> TearThroughSpace
tearThroughSpace lid = TearThroughSpace $ baseAttrs
  lid
  "02324"
  (Name "Tear Through Space" Nothing)
  EncounterSet.LostInTimeAndSpace
  1
  (Static 1)
  Square
  [Diamond, Triangle, Square]
  [Otherworld, Extradimensional]

instance HasModifiersFor env TearThroughSpace where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env TearThroughSpace where
  getActions iid window (TearThroughSpace attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env TearThroughSpace where
  runMessage msg (TearThroughSpace attrs) =
    TearThroughSpace <$> runMessage msg attrs
