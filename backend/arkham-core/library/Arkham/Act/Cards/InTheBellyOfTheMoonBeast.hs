module Arkham.Act.Cards.InTheBellyOfTheMoonBeast
  ( InTheBellyOfTheMoonBeast(..)
  , inTheBellyOfTheMoonBeast
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype InTheBellyOfTheMoonBeast = InTheBellyOfTheMoonBeast ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

inTheBellyOfTheMoonBeast :: ActCard InTheBellyOfTheMoonBeast
inTheBellyOfTheMoonBeast = act (1, A) InTheBellyOfTheMoonBeast Cards.inTheBellyOfTheMoonBeast Nothing

instance RunMessage InTheBellyOfTheMoonBeast where
  runMessage msg (InTheBellyOfTheMoonBeast attrs) = InTheBellyOfTheMoonBeast <$> runMessage msg attrs
