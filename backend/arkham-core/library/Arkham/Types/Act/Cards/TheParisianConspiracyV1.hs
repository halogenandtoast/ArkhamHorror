module Arkham.Types.Act.Cards.TheParisianConspiracyV1
  ( TheParisianConspiracyV1(..)
  , theParisianConspiracyV1
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes

newtype TheParisianConspiracyV1 = TheParisianConspiracyV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theParisianConspiracyV1 :: ActCard TheParisianConspiracyV1
theParisianConspiracyV1 =
  act (1, A) TheParisianConspiracyV1 Cards.theParisianConspiracyV1 Nothing

instance ActRunner env => RunMessage env TheParisianConspiracyV1 where
  runMessage msg (TheParisianConspiracyV1 attrs) =
    TheParisianConspiracyV1 <$> runMessage msg attrs
