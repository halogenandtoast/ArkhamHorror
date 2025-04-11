module Arkham.Location.Cards.BackAlley (backAlley) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (backAlley)
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheHouseAlwaysWins.Helpers

newtype BackAlley = BackAlley LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backAlley :: LocationCard BackAlley
backAlley = location BackAlley Cards.backAlley 1 (PerPlayer 1)

instance HasAbilities BackAlley where
  getAbilities (BackAlley a) = extendRevealed1 a $ scenarioI18n $ withI18nTooltip "backAlley.resign" $ locationResignAction a

instance RunMessage BackAlley where
  runMessage msg (BackAlley attrs) = BackAlley <$> runMessage msg attrs
