{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ScienceBuilding where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ScienceBuilding = ScienceBuilding Attrs
  deriving newtype (Show, ToJSON, FromJSON)

scienceBuilding :: ScienceBuilding
scienceBuilding = ScienceBuilding $ baseAttrs
  "02056"
  "Science Building"
  EncounterSet.ExtracurricularActivity
  2
  (PerPlayer 1)
  Hourglass
  [Plus, Squiggle]
  [Miskatonic]

instance HasModifiersFor env ScienceBuilding where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ScienceBuilding where
  getActions i window (ScienceBuilding attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ScienceBuilding where
  runMessage msg (ScienceBuilding attrs) =
    ScienceBuilding <$> runMessage msg attrs
