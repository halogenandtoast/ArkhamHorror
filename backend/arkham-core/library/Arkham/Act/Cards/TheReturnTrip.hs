module Arkham.Act.Cards.TheReturnTrip
  ( TheReturnTrip(..)
  , theReturnTrip
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheReturnTrip = TheReturnTrip ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theReturnTrip :: ActCard TheReturnTrip
theReturnTrip = act (3, A) TheReturnTrip Cards.theReturnTrip Nothing

instance RunMessage TheReturnTrip where
  runMessage msg (TheReturnTrip attrs) = TheReturnTrip <$> runMessage msg attrs
