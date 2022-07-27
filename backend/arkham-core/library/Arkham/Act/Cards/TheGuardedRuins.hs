module Arkham.Act.Cards.TheGuardedRuins
  ( TheGuardedRuins(..)
  , theGuardedRuins
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheGuardedRuins = TheGuardedRuins ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGuardedRuins :: ActCard TheGuardedRuins
theGuardedRuins = act (3, A) TheGuardedRuins Cards.theGuardedRuins Nothing

instance RunMessage TheGuardedRuins where
  runMessage msg (TheGuardedRuins attrs) = TheGuardedRuins <$> runMessage msg attrs
