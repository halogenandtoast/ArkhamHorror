{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.HoleInTheWall where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype HoleInTheWall = HoleInTheWall Attrs
  deriving newtype (Show, ToJSON, FromJSON)

holeInTheWall :: HoleInTheWall
holeInTheWall = HoleInTheWall $ baseAttrs
  "50017"
  "Hole in the Wall"
  EncounterSet.ReturnToTheGathering
  1
  (Static 0)
  Square
  [T, Triangle, Plus, Diamond]
  mempty

instance HasModifiersFor env HoleInTheWall where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env HoleInTheWall where
  getActions i window (HoleInTheWall attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env HoleInTheWall where
  runMessage msg (HoleInTheWall attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      unshiftMessages
        [ PlaceLocationNamed "Attic"
        , PlaceLocationNamed "Cellar"
        , PlaceLocationNamed "Parlor"
        ]
      HoleInTheWall <$> runMessage msg attrs
    _ -> HoleInTheWall <$> runMessage msg attrs
