module Arkham.Types.Location.Cards.MiskatonicQuad
  ( MiskatonicQuad(..)
  , miskatonicQuad
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (miskatonicQuad)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol

newtype MiskatonicQuad = MiskatonicQuad LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicQuad :: LocationCard MiskatonicQuad
miskatonicQuad = location
  MiskatonicQuad
  Cards.miskatonicQuad
  3
  (Static 0)
  Plus
  [Triangle, Hourglass, Square, Diamond, Circle]

instance HasModifiersFor env MiskatonicQuad

instance ActionRunner env => HasActions env MiskatonicQuad where
  getActions = withResignAction

instance (LocationRunner env) => RunMessage env MiskatonicQuad where
  runMessage msg (MiskatonicQuad attrs) =
    MiskatonicQuad <$> runMessage msg attrs
