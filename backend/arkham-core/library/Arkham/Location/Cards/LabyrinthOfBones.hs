module Arkham.Location.Cards.LabyrinthOfBones
  ( labyrinthOfBones
  , LabyrinthOfBones(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype LabyrinthOfBones = LabyrinthOfBones LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

labyrinthOfBones :: LocationCard LabyrinthOfBones
labyrinthOfBones = location LabyrinthOfBones Cards.labyrinthOfBones 0 (Static 0) NoSymbol []

instance HasAbilities LabyrinthOfBones where
  getAbilities (LabyrinthOfBones attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance LocationRunner env => RunMessage env LabyrinthOfBones where
  runMessage msg (LabyrinthOfBones attrs) =
    LabyrinthOfBones <$> runMessage msg attrs
