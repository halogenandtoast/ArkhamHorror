{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.StudentUnion where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype StudentUnion = StudentUnion Attrs
  deriving newtype (Show, ToJSON, FromJSON)

studentUnion :: StudentUnion
studentUnion = StudentUnion $ baseAttrs
  "02051"
  "Student Union"
  EncounterSet.ExtracurricularActivity
  1
  (Static 2)
  Diamond
  [Plus, Equals]
  [Miskatonic]

instance HasModifiersFor env StudentUnion where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env StudentUnion where
  getActions i window (StudentUnion attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env StudentUnion where
  runMessage msg (StudentUnion attrs) =
    StudentUnion <$> runMessage msg attrs
