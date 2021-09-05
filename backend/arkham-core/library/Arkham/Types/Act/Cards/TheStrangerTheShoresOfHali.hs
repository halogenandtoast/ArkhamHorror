module Arkham.Types.Act.Cards.TheStrangerTheShoresOfHali
  ( TheStrangerTheShoresOfHali(..)
  , theStrangerTheShoresOfHali
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes

newtype TheStrangerTheShoresOfHali = TheStrangerTheShoresOfHali ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerTheShoresOfHali :: ActCard TheStrangerTheShoresOfHali
theStrangerTheShoresOfHali =
  act (2, A) TheStrangerTheShoresOfHali Cards.theStrangerTheShoresOfHali Nothing

instance ActRunner env => RunMessage env TheStrangerTheShoresOfHali where
  runMessage msg (TheStrangerTheShoresOfHali attrs) =
    TheStrangerTheShoresOfHali <$> runMessage msg attrs
