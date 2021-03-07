module Arkham.Types.Location.Cards.AnotherDimension
  ( anotherDimension
  , AnotherDimension(..)
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

newtype AnotherDimension = AnotherDimension LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anotherDimension :: LocationId -> AnotherDimension
anotherDimension lid = AnotherDimension $ baseAttrs
  lid
  "02320"
  (Name "Another Dimension" (Just "Unfettered by Reality"))
  EncounterSet.LostInTimeAndSpace
  6
  (Static 0)
  Circle
  [Square, Diamond, Triangle]
  [Otherworld]

instance HasModifiersFor env AnotherDimension where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env AnotherDimension where
  getActions iid window (AnotherDimension attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env AnotherDimension where
  runMessage msg (AnotherDimension attrs) =
    AnotherDimension <$> runMessage msg attrs
