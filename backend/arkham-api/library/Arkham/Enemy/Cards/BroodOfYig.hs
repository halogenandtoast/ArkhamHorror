module Arkham.Enemy.Cards.BroodOfYig (broodOfYig, BroodOfYig (..)) where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner hiding (EnemyFight)
import Arkham.Helpers.Modifiers
import Arkham.Prelude

newtype BroodOfYig = BroodOfYig EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

broodOfYig :: EnemyCard BroodOfYig
broodOfYig = enemy BroodOfYig Cards.broodOfYig (2, Static 3, 2) (1, 1)

instance HasModifiersFor BroodOfYig where
  getModifiersFor (BroodOfYig a) = do
    vengeance <- getVengeanceInVictoryDisplay
    modifySelfWhen a (vengeance > 0) [EnemyFight vengeance]

instance RunMessage BroodOfYig where
  runMessage msg (BroodOfYig attrs) =
    BroodOfYig <$> runMessage msg attrs
