module Arkham.Act.Cards.TheYithianRelic
  ( TheYithianRelic(..)
  , theYithianRelic
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheYithianRelic = TheYithianRelic ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theYithianRelic :: ActCard TheYithianRelic
theYithianRelic = act (3, A) TheYithianRelic Cards.theYithianRelic Nothing

instance RunMessage TheYithianRelic where
  runMessage msg (TheYithianRelic attrs) =
    TheYithianRelic <$> runMessage msg attrs
