module Arkham.Location.Cards.BoneFilledCavern
  ( boneFilledCavern
  , BoneFilledCavern(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype BoneFilledCavern = BoneFilledCavern LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boneFilledCavern :: LocationCard BoneFilledCavern
boneFilledCavern = location BoneFilledCavern Cards.boneFilledCavern 0 (Static 0) NoSymbol []

instance HasAbilities BoneFilledCavern where
  getAbilities (BoneFilledCavern attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance LocationRunner env => RunMessage env BoneFilledCavern where
  runMessage msg (BoneFilledCavern attrs) =
    BoneFilledCavern <$> runMessage msg attrs
