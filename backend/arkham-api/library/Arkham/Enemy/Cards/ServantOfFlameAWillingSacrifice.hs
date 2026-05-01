module Arkham.Enemy.Cards.ServantOfFlameAWillingSacrifice (servantOfFlameAWillingSacrifice) where

import Arkham.Ability
import Arkham.Campaigns.BrethrenOfAsh.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Matcher

newtype ServantOfFlameAWillingSacrifice = ServantOfFlameAWillingSacrifice EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfFlameAWillingSacrifice :: EnemyCard ServantOfFlameAWillingSacrifice
servantOfFlameAWillingSacrifice =
  enemy
    ServantOfFlameAWillingSacrifice
    Cards.servantOfFlameAWillingSacrifice
    (4, PerPlayer 5, 4)
    (2, 2)

instance HasAbilities ServantOfFlameAWillingSacrifice where
  getAbilities (ServantOfFlameAWillingSacrifice a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #after You ByAny (be a)

instance RunMessage ServantOfFlameAWillingSacrifice where
  runMessage msg e@(ServantOfFlameAWillingSacrifice attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) Phi
      pure e
    _ -> ServantOfFlameAWillingSacrifice <$> liftRunMessage msg attrs
