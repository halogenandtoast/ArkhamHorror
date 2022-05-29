module Arkham.Act.Cards.TheWayOut
  ( TheWayOut(..)
  , theWayOut
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Attrs
import Arkham.Act.Runner
import Arkham.Classes

newtype TheWayOut = TheWayOut ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWayOut :: ActCard TheWayOut
theWayOut = act (1, A) TheWayOut Cards.theWayOut Nothing

instance ActRunner env => RunMessage env TheWayOut where
  runMessage msg (TheWayOut attrs) = TheWayOut <$> runMessage msg attrs
