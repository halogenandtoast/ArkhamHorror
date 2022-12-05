module Arkham.Act.Cards.Timelock
  ( Timelock(..)
  , timelock
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype Timelock = Timelock ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

timelock :: ActCard Timelock
timelock = act (4, A) Timelock Cards.timelock Nothing

instance RunMessage Timelock where
  runMessage msg (Timelock attrs) = Timelock <$> runMessage msg attrs
