module Arkham.Types.Location.Cards.MiskatonicQuad
  ( MiskatonicQuad(..)
  , miskatonicQuad
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (miskatonicQuad)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol

newtype MiskatonicQuad = MiskatonicQuad LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicQuad :: LocationId -> MiskatonicQuad
miskatonicQuad = MiskatonicQuad . baseAttrs
  Cards.miskatonicQuad
  3
  (Static 0)
  Plus
  [Triangle, Hourglass, Square, Diamond, Circle]

instance HasModifiersFor env MiskatonicQuad where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MiskatonicQuad where
  getActions = withResignAction

instance (LocationRunner env) => RunMessage env MiskatonicQuad where
  runMessage msg (MiskatonicQuad attrs) =
    MiskatonicQuad <$> runMessage msg attrs
