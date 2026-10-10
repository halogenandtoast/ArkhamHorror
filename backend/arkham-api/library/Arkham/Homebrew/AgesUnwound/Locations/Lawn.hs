module Arkham.Homebrew.AgesUnwound.Locations.Lawn (lawn) where

import Arkham.Ability (withI18nTooltip)
import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Location.Import.Lifted

newtype Lawn = Lawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lawn :: LocationCard Lawn
lawn = symbolLabel $ location Lawn Cards.lawn 1 (PerPlayer 1)

-- | "[action]: Resign. Coming here was a bad idea."
instance HasAbilities Lawn where
  getAbilities (Lawn a) =
    extendRevealed1 a $ scenarioI18n $ withI18nTooltip "lawn.resign" $ locationResignAction a

instance RunMessage Lawn where
  runMessage msg (Lawn attrs) = Lawn <$> runMessage msg attrs
