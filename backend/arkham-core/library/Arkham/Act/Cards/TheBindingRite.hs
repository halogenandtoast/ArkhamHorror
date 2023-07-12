module Arkham.Act.Cards.TheBindingRite
  ( TheBindingRite(..)
  , theBindingRite
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheBindingRite = TheBindingRite ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theBindingRite :: ActCard TheBindingRite
theBindingRite = act (4, A) TheBindingRite Cards.theBindingRite Nothing

instance RunMessage TheBindingRite where
  runMessage msg (TheBindingRite attrs) = TheBindingRite <$> runMessage msg attrs
