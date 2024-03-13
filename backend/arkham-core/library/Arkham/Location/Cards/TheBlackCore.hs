module Arkham.Location.Cards.TheBlackCore (
  theBlackCore,
  TheBlackCore (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TheBlackCore = TheBlackCore LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackCore :: LocationCard TheBlackCore
theBlackCore = location TheBlackCore Cards.theBlackCore 0 (PerPlayer 1)

instance HasAbilities TheBlackCore where
  getAbilities (TheBlackCore attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage TheBlackCore where
  runMessage msg (TheBlackCore attrs) =
    TheBlackCore <$> runMessage msg attrs
