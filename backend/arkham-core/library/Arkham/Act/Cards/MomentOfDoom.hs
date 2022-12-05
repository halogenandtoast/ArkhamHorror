module Arkham.Act.Cards.MomentOfDoom
  ( MomentOfDoom(..)
  , momentOfDoom
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype MomentOfDoom = MomentOfDoom ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

momentOfDoom :: ActCard MomentOfDoom
momentOfDoom = act (3, A) MomentOfDoom Cards.momentOfDoom Nothing

instance RunMessage MomentOfDoom where
  runMessage msg (MomentOfDoom attrs) = MomentOfDoom <$> runMessage msg attrs
