module Arkham.Location.Cards.Foyer (foyer) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheLastKing.Helpers

newtype Foyer = Foyer LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foyer :: LocationCard Foyer
foyer = location Foyer Cards.foyer 2 (PerPlayer 1)

instance HasAbilities Foyer where
  getAbilities (Foyer a) =
    extendRevealed1 a $ scenarioI18n $ withI18nTooltip "foyer.resign" $ locationResignAction a

instance RunMessage Foyer where
  runMessage msg (Foyer attrs) = Foyer <$> runMessage msg attrs
