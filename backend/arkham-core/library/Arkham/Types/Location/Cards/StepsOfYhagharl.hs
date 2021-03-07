module Arkham.Types.Location.Cards.StepsOfYhagharl
  ( stepsOfYhagharl
  , StepsOfYhagharl(..)
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

newtype StepsOfYhagharl = StepsOfYhagharl LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stepsOfYhagharl :: LocationId -> StepsOfYhagharl
stepsOfYhagharl lid = StepsOfYhagharl $ baseAttrs
  lid
  "02327"
  (Name "Steps of Y'hagharl" Nothing)
  EncounterSet.LostInTimeAndSpace
  3
  (PerPlayer 1)
  Plus
  [Diamond, Moon]
  [Otherworld, Extradimensional]

instance HasModifiersFor env StepsOfYhagharl where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env StepsOfYhagharl where
  getActions iid window (StepsOfYhagharl attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env StepsOfYhagharl where
  runMessage msg (StepsOfYhagharl attrs) =
    StepsOfYhagharl <$> runMessage msg attrs
