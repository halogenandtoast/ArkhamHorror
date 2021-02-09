module Arkham.Types.Location.Cards.MiskatonicQuad
  ( MiskatonicQuad(..)
  , miskatonicQuad
  ) where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype MiskatonicQuad = MiskatonicQuad LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicQuad :: MiskatonicQuad
miskatonicQuad = MiskatonicQuad $ baseAttrs
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
