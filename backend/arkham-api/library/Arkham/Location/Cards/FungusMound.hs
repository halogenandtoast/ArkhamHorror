module Arkham.Location.Cards.FungusMound (fungusMound) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FungusMound = FungusMound LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

fungusMound :: LocationCard FungusMound
fungusMound = locationWith FungusMound Cards.fungusMound 5 (Static 0) connectsToAdjacent

instance RunMessage FungusMound where
  runMessage msg (FungusMound attrs) = FungusMound <$> runMessage msg attrs
