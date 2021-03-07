module Arkham.Types.Location.Cards.VillageCommons
  ( villageCommons
  , VillageCommons(..)
  ) where

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

newtype VillageCommons = VillageCommons LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

villageCommons :: LocationId -> VillageCommons
villageCommons lid = VillageCommons $ baseAttrs
  lid
  "02201"
  (Name "Village Commons" Nothing)
  EncounterSet.BloodOnTheAltar
  3
  (Static 0)
  Plus
  [Square, Circle, Moon]
  [Dunwich, Central]

instance HasModifiersFor env VillageCommons where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env VillageCommons where
  getActions = withResignAction

instance LocationRunner env => RunMessage env VillageCommons where
  runMessage msg (VillageCommons attrs) =
    VillageCommons <$> runMessage msg attrs
