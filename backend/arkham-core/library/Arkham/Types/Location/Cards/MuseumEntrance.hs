module Arkham.Types.Location.Cards.MuseumEntrance
  ( museumEntrance
  , MuseumEntrance(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype MuseumEntrance = MuseumEntrance Attrs
  deriving newtype (Show, ToJSON, FromJSON)

museumEntrance :: MuseumEntrance
museumEntrance = MuseumEntrance $ baseAttrs
  "02126"
  (LocationName "Museum Entrance" Nothing)
  EncounterSet.TheMiskatonicMuseum
  3
  (Static 2)
  Circle
  [Square]
  (singleton Miskatonic)

instance HasModifiersFor env MuseumEntrance where
  getModifiersFor _ (InvestigatorTarget iid) (MuseumEntrance attrs) =
    pure $ toModifiers attrs [ CannotGainResources | iid `on` attrs ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env MuseumEntrance where
  getActions iid NonFast (MuseumEntrance attrs) | locationRevealed attrs =
    withBaseActions iid NonFast attrs
      $ pure [ resignAction iid attrs | iid `on` attrs ]
  getActions i window (MuseumEntrance attrs) = getActions i window attrs

instance LocationRunner env => RunMessage env MuseumEntrance where
  runMessage msg (MuseumEntrance attrs) =
    MuseumEntrance <$> runMessage msg attrs
