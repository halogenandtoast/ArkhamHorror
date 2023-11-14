module Arkham.Location.Cards.SevenHundredSteps (
  sevenHundredSteps,
  SevenHundredSteps (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SevenHundredSteps = SevenHundredSteps LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sevenHundredSteps :: LocationCard SevenHundredSteps
sevenHundredSteps = location SevenHundredSteps Cards.sevenHundredSteps 2 (PerPlayer 1)

instance HasAbilities SevenHundredSteps where
  getAbilities (SevenHundredSteps attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage SevenHundredSteps where
  runMessage msg (SevenHundredSteps attrs) =
    SevenHundredSteps <$> runMessage msg attrs
