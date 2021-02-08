module Arkham.Types.Location.Cards.HouseInTheReeds_211
  ( houseInTheReeds_211
  , HouseInTheReeds_211(..)
  ) where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype HouseInTheReeds_211 = HouseInTheReeds_211 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

houseInTheReeds_211 :: HouseInTheReeds_211
houseInTheReeds_211 = HouseInTheReeds_211 $ baseAttrs
  "02211"
  (Name "House in the Reeds" Nothing)
  EncounterSet.BloodOnTheAltar
  1
  (PerPlayer 1)
  Squiggle
  [Diamond, Moon]
  [Dunwich]

instance HasModifiersFor env HouseInTheReeds_211 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env HouseInTheReeds_211 where
  getActions = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env HouseInTheReeds_211 where
  runMessage msg (HouseInTheReeds_211 attrs) =
    HouseInTheReeds_211 <$> runMessage msg attrs
