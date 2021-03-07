module Arkham.Types.Location.Cards.MiskatonicQuad
  ( MiskatonicQuad(..)
  , miskatonicQuad
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Name
import Arkham.Types.Trait

newtype MiskatonicQuad = MiskatonicQuad LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicQuad :: LocationId -> MiskatonicQuad
miskatonicQuad lid = MiskatonicQuad $ baseAttrs
  lid
  "02048"
  (Name "Miskatonic Quad" Nothing)
  EncounterSet.ExtracurricularActivity
  3
  (Static 0)
  Plus
  [Triangle, Hourglass, Square, Diamond, Circle]
  [Miskatonic]

instance HasModifiersFor env MiskatonicQuad where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MiskatonicQuad where
  getActions = withResignAction

instance (LocationRunner env) => RunMessage env MiskatonicQuad where
  runMessage msg (MiskatonicQuad attrs) =
    MiskatonicQuad <$> runMessage msg attrs
