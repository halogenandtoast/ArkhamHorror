module Arkham.Act.Cards.TheSpectralRealm
  ( TheSpectralRealm(..)
  , theSpectralRealm
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher

newtype TheSpectralRealm = TheSpectralRealm ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theSpectralRealm :: ActCard TheSpectralRealm
theSpectralRealm = act (2, A) TheSpectralRealm Cards.theSpectralRealm (Just $ GroupClueCost (PerPlayer 4) Anywhere)

instance RunMessage TheSpectralRealm where
  runMessage msg (TheSpectralRealm attrs) = TheSpectralRealm <$> runMessage msg attrs
