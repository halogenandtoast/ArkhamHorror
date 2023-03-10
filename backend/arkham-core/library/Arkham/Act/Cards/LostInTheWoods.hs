module Arkham.Act.Cards.LostInTheWoods
  ( LostInTheWoods(..)
  , lostInTheWoods
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype LostInTheWoods = LostInTheWoods ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lostInTheWoods :: ActCard LostInTheWoods
lostInTheWoods = act (1, A) LostInTheWoods Cards.lostInTheWoods Nothing

instance RunMessage LostInTheWoods where
  runMessage msg (LostInTheWoods attrs) = LostInTheWoods <$> runMessage msg attrs
