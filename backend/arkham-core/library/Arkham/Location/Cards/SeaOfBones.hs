module Arkham.Location.Cards.SeaOfBones
  ( seaOfBones
  , SeaOfBones(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SeaOfBones = SeaOfBones LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seaOfBones :: LocationCard SeaOfBones
seaOfBones = location SeaOfBones Cards.seaOfBones 2 (PerPlayer 1)

instance HasAbilities SeaOfBones where
  getAbilities (SeaOfBones attrs) =
    extendRevealed attrs []

instance RunMessage SeaOfBones where
  runMessage msg (SeaOfBones attrs) =
    SeaOfBones <$> runMessage msg attrs
