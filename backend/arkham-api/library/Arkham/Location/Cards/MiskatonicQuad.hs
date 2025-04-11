module Arkham.Location.Cards.MiskatonicQuad (miskatonicQuad) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (miskatonicQuad)
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.ExtracurricularActivity.Helpers

newtype MiskatonicQuad = MiskatonicQuad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicQuad :: LocationCard MiskatonicQuad
miskatonicQuad = symbolLabel $ location MiskatonicQuad Cards.miskatonicQuad 3 (Static 0)

instance HasAbilities MiskatonicQuad where
  getAbilities (MiskatonicQuad a) =
    extendRevealed1 a $ scenarioI18n $ withI18nTooltip "miskatonicQuad.resign" $ locationResignAction a

instance RunMessage MiskatonicQuad where
  runMessage msg (MiskatonicQuad attrs) = MiskatonicQuad <$> runMessage msg attrs
