module Arkham.Enemy.Cards.ServantOfFlameOnTheRun (servantOfFlameOnTheRun) where

import Arkham.Ability
import Arkham.Campaigns.BrethrenOfAsh.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Matcher

newtype ServantOfFlameOnTheRun = ServantOfFlameOnTheRun EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfFlameOnTheRun :: EnemyCard ServantOfFlameOnTheRun
servantOfFlameOnTheRun =
  enemy ServantOfFlameOnTheRun Cards.servantOfFlameOnTheRun (4, PerPlayer 5, 4) (2, 2)

instance HasAbilities ServantOfFlameOnTheRun where
  getAbilities (ServantOfFlameOnTheRun a) =
    extend1 a
      $ mkAbility a 1
      $ SilentForcedAbility
      $ EnemyDefeated #after You ByAny (be a)

instance RunMessage ServantOfFlameOnTheRun where
  runMessage msg e@(ServantOfFlameOnTheRun attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 7
      pure e
    _ -> ServantOfFlameOnTheRun <$> liftRunMessage msg attrs
