module Arkham.Location.Cards.BurnedRuins_204 (burnedRuins_204, BurnedRuins_204 (..)) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (burnedRuins_204)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Location.Runner (withDrawCardUnderneathAction)
import Arkham.Matcher

newtype BurnedRuins_204 = BurnedRuins_204 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burnedRuins_204 :: LocationCard BurnedRuins_204
burnedRuins_204 = location BurnedRuins_204 Cards.burnedRuins_204 3 (Static 3)

instance HasModifiersFor BurnedRuins_204 where
  getModifiersFor (BurnedRuins_204 a) =
    whenRevealed a $ modifySelect a (enemyAt a) [EnemyEvade 1]

instance HasAbilities BurnedRuins_204 where
  getAbilities = withDrawCardUnderneathAction

instance RunMessage BurnedRuins_204 where
  runMessage msg (BurnedRuins_204 attrs) =
    BurnedRuins_204 <$> runMessage msg attrs
