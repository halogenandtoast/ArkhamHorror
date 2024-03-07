module Arkham.Act.Cards.TheEndlessStairs
  ( TheEndlessStairs(..)
  , theEndlessStairs
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheEndlessStairs = TheEndlessStairs ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theEndlessStairs :: ActCard TheEndlessStairs
theEndlessStairs = act (2, A) TheEndlessStairs Cards.theEndlessStairs Nothing

instance RunMessage TheEndlessStairs where
  runMessage msg (TheEndlessStairs attrs) = TheEndlessStairs <$> runMessage msg attrs
