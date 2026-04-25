module Arkham.Enemy.Cards.CorneliaAkelyExhaustedSupervisor (corneliaAkelyExhaustedSupervisor) where

import Arkham.Ability
import Arkham.Campaigns.BrethrenOfAsh.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Matcher (be, pattern EnemyEvaded, pattern You)

newtype CorneliaAkelyExhaustedSupervisor = CorneliaAkelyExhaustedSupervisor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corneliaAkelyExhaustedSupervisor :: EnemyCard CorneliaAkelyExhaustedSupervisor
corneliaAkelyExhaustedSupervisor =
  enemy CorneliaAkelyExhaustedSupervisor Cards.corneliaAkelyExhaustedSupervisor (3, Static 4, 3) (1, 1)

instance HasAbilities CorneliaAkelyExhaustedSupervisor where
  getAbilities (CorneliaAkelyExhaustedSupervisor a) =
    extend1 a
      $ mkAbility a 1
      $ freeReaction
      $ EnemyEvaded #after You (be a)

instance RunMessage CorneliaAkelyExhaustedSupervisor where
  runMessage msg e@(CorneliaAkelyExhaustedSupervisor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readyThis attrs
      codex iid (attrs.ability 1) 2
      pure e
    _ -> CorneliaAkelyExhaustedSupervisor <$> liftRunMessage msg attrs
