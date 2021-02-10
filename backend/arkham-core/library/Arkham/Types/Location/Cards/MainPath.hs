module Arkham.Types.Location.Cards.MainPath
  ( MainPath(..)
  , mainPath
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype MainPath = MainPath LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mainPath :: MainPath
mainPath = MainPath $ baseAttrs
  "01149"
  (Name "Main Path" Nothing)
  EncounterSet.TheDevourerBelow
  2
  (Static 0)
  Squiggle
  [Square, Plus]
  [Woods]

instance HasModifiersFor env MainPath where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MainPath where
  getActions = withResignAction

instance (LocationRunner env) => RunMessage env MainPath where
  runMessage msg l@(MainPath attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage (Resign iid)
    AddConnection lid _ | locationId /= lid -> do
      isWoods <- member Woods <$> getSet lid
      if isWoods
        then MainPath
          <$> runMessage msg (attrs & connectedLocationsL %~ insertSet lid)
        else MainPath <$> runMessage msg attrs
    _ -> MainPath <$> runMessage msg attrs
