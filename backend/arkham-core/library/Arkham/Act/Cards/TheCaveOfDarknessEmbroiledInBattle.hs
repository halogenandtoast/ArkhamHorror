module Arkham.Act.Cards.TheCaveOfDarknessEmbroiledInBattle
  ( TheCaveOfDarknessEmbroiledInBattle(..)
  , theCaveOfDarknessEmbroiledInBattle
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheCaveOfDarknessEmbroiledInBattle = TheCaveOfDarknessEmbroiledInBattle ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theCaveOfDarknessEmbroiledInBattle
  :: ActCard TheCaveOfDarknessEmbroiledInBattle
theCaveOfDarknessEmbroiledInBattle = act
  (2, E)
  TheCaveOfDarknessEmbroiledInBattle
  Cards.theCaveOfDarknessEmbroiledInBattle
  Nothing

instance RunMessage TheCaveOfDarknessEmbroiledInBattle where
  runMessage msg (TheCaveOfDarknessEmbroiledInBattle attrs) =
    TheCaveOfDarknessEmbroiledInBattle <$> runMessage msg attrs
