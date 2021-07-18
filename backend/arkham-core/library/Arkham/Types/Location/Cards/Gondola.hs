module Arkham.Types.Location.Cards.Gondola
  ( gondola
  , Gondola(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype Gondola = Gondola LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gondola :: LocationCard Gondola
gondola = location Gondola Cards.gondola 5 (Static 0) NoSymbol []

instance HasModifiersFor env Gondola

instance ActionRunner env => HasActions env Gondola where
  getActions iid window (Gondola attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env Gondola where
  runMessage msg l@(Gondola attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      locationIds <-
        setToList . deleteSet (toId attrs) <$> getSet @LocationId ()
      l <$ pushAll
        (MoveAllTo (toId attrs) : [ RemoveLocation lid | lid <- locationIds ])
    _ -> Gondola <$> runMessage msg attrs
