module Arkham.Location.Cards.AvenidaDeMayo (avenidaDeMayo) where

import Arkham.Ability
import Arkham.Difficulty
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Scenario (getDifficulty)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.SanguineShadows.Helpers

newtype AvenidaDeMayo = AvenidaDeMayo LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

avenidaDeMayo :: LocationCard AvenidaDeMayo
avenidaDeMayo = setLabel "a" $ location AvenidaDeMayo Cards.avenidaDeMayo 0 (PerPlayer 1)

instance HasAbilities AvenidaDeMayo where
  getAbilities (AvenidaDeMayo a) =
    extendRevealed1 a $ scenarioI18n $ withI18nTooltip "avenidaDeMayo.resign" $ locationResignAction a

instance HasModifiersFor AvenidaDeMayo where
  getModifiersFor (AvenidaDeMayo a) = do
    shroud <-
      getDifficulty <&> \case
        Easy -> 2
        Standard -> 3
        Hard -> 4
        Expert -> 5
    modifySelf a [ShroudModifier shroud]

instance RunMessage AvenidaDeMayo where
  runMessage msg (AvenidaDeMayo attrs) = AvenidaDeMayo <$> runMessage msg attrs
