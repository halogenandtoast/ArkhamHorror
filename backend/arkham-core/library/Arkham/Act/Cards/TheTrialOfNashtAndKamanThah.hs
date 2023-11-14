module Arkham.Act.Cards.TheTrialOfNashtAndKamanThah
  ( TheTrialOfNashtAndKamanThah(..)
  , theTrialOfNashtAndKamanThah
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheTrialOfNashtAndKamanThah = TheTrialOfNashtAndKamanThah ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theTrialOfNashtAndKamanThah :: ActCard TheTrialOfNashtAndKamanThah
theTrialOfNashtAndKamanThah = act (2, A) TheTrialOfNashtAndKamanThah Cards.theTrialOfNashtAndKamanThah Nothing

instance RunMessage TheTrialOfNashtAndKamanThah where
  runMessage msg (TheTrialOfNashtAndKamanThah attrs) = TheTrialOfNashtAndKamanThah <$> runMessage msg attrs
