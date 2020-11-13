{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.HoleInTheWall where

import Arkham.Import
import Arkham.Types.Helpers (sample)
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Data.List.NonEmpty (NonEmpty(..))

newtype HoleInTheWall = HoleInTheWall Attrs
  deriving newtype (Show, ToJSON, FromJSON)

holeInTheWall :: HoleInTheWall
holeInTheWall = HoleInTheWall $ baseAttrs
  "50017"
  "Hole in the Wall"
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
      attic <- liftIO $ sample ("50018" :| ["01113"])
      cellar <- liftIO $ sample ("50020" :| ["01114"])
      unshiftMessages
        [PlaceLocation attic, PlaceLocation cellar, PlaceLocation "01115"]
      HoleInTheWall <$> runMessage msg attrs
    _ -> HoleInTheWall <$> runMessage msg attrs
