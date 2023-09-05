module Arkham.Act.Cards.NewWorldOrder
  ( NewWorldOrder(..)
  , newWorldOrder
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype NewWorldOrder = NewWorldOrder ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

newWorldOrder :: ActCard NewWorldOrder
newWorldOrder = act (2, A) NewWorldOrder Cards.newWorldOrder Nothing

instance RunMessage NewWorldOrder where
  runMessage msg (NewWorldOrder attrs) = NewWorldOrder <$> runMessage msg attrs
