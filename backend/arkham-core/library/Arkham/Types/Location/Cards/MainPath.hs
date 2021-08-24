module Arkham.Types.Location.Cards.MainPath
  ( MainPath(..)
  , mainPath
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (mainPath)
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message
import Arkham.Types.Trait

newtype MainPath = MainPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mainPath :: LocationCard MainPath
mainPath =
  location MainPath Cards.mainPath 2 (Static 0) Squiggle [Square, Plus]

instance HasAbilities env MainPath where
  getAbilities iid window (MainPath a) =
    withBaseAbilities iid window a $ pure [locationResignAction a]

-- TODO: make constant ability "connected to woods" a modifier
instance LocationRunner env => RunMessage env MainPath where
  runMessage msg l@(MainPath attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (Resign iid)
    AddConnection lid _ | locationId /= lid -> do
      isWoods <- member Woods <$> getSet lid
      if isWoods
        then MainPath
          <$> runMessage msg (attrs & connectedLocationsL %~ insertSet lid)
        else MainPath <$> runMessage msg attrs
    _ -> MainPath <$> runMessage msg attrs
