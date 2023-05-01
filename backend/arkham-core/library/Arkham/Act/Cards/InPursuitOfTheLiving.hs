module Arkham.Act.Cards.InPursuitOfTheLiving
  ( InPursuitOfTheLiving(..)
  , inPursuitOfTheLiving
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype InPursuitOfTheLiving = InPursuitOfTheLiving ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

inPursuitOfTheLiving :: ActCard InPursuitOfTheLiving
inPursuitOfTheLiving = act (2, A) InPursuitOfTheLiving Cards.inPursuitOfTheLiving Nothing

instance RunMessage InPursuitOfTheLiving where
  runMessage msg (InPursuitOfTheLiving attrs) = InPursuitOfTheLiving <$> runMessage msg attrs
