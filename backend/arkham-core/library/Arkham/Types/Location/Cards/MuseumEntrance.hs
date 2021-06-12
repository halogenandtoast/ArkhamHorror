module Arkham.Types.Location.Cards.MuseumEntrance
  ( museumEntrance
  , MuseumEntrance(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Trait

newtype MuseumEntrance = MuseumEntrance LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

museumEntrance :: LocationId -> MuseumEntrance
museumEntrance = MuseumEntrance . baseAttrs
  "02126"
  "Museum Entrance"
  EncounterSet.TheMiskatonicMuseum
  3
  (Static 2)
  Circle
  [Square]
  [Miskatonic]

instance HasModifiersFor env MuseumEntrance where
  getModifiersFor _ (InvestigatorTarget iid) (MuseumEntrance location) =
    pure $ toModifiers location [ CannotGainResources | iid `on` location ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env MuseumEntrance where
  getActions = withResignAction

instance LocationRunner env => RunMessage env MuseumEntrance where
  runMessage msg (MuseumEntrance location) =
    MuseumEntrance <$> runMessage msg location
