module Arkham.Types.Act.Cards.TheParisianConspiracyV2
  ( TheParisianConspiracyV2(..)
  , theParisianConspiracyV2
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes

newtype TheParisianConspiracyV2 = TheParisianConspiracyV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theParisianConspiracyV2 :: ActCard TheParisianConspiracyV2
theParisianConspiracyV2 =
  act (1, A) TheParisianConspiracyV2 Cards.theParisianConspiracyV2 Nothing

instance ActRunner env => RunMessage env TheParisianConspiracyV2 where
  runMessage msg (TheParisianConspiracyV2 attrs) =
    TheParisianConspiracyV2 <$> runMessage msg attrs
