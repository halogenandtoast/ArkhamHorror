{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.MiskatonicQuad where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype MiskatonicQuad = MiskatonicQuad Attrs
  deriving newtype (Show, ToJSON, FromJSON)

miskatonicQuad :: MiskatonicQuad
miskatonicQuad = MiskatonicQuad $ baseAttrs
  "02048"
  "Miskatonic Quad"
  EncounterSet.ExtracurricularActivity
  3
  (Static 0)
  Plus
  [Triangle, Hourglass, Square, Diamond, Circle]
  [Miskatonic]

instance HasModifiersFor env MiskatonicQuad where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MiskatonicQuad where
  getActions i window (MiskatonicQuad attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env MiskatonicQuad where
  runMessage msg (MiskatonicQuad attrs) =
    MiskatonicQuad <$> runMessage msg attrs
