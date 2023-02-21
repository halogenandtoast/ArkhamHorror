module Arkham.Act.Cards.TheDisappearance
  ( TheDisappearance(..)
  , theDisappearance
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheDisappearance = TheDisappearance ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theDisappearance :: ActCard TheDisappearance
theDisappearance = act (1, A) TheDisappearance Cards.theDisappearance Nothing

instance RunMessage TheDisappearance where
  runMessage msg (TheDisappearance attrs) = TheDisappearance <$> runMessage msg attrs
