module Arkham.Enemy.Cards.RuthTurner (ruthTurner) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Matcher

newtype RuthTurner = RuthTurner EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruthTurner :: EnemyCard RuthTurner
ruthTurner = enemyWith RuthTurner Cards.ruthTurner (2, Static 4, 5) (1, 0) (spawnAtL ?~ "St. Mary's Hospital")

instance HasAbilities RuthTurner where
  getAbilities (RuthTurner a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEvaded #after Anyone (be a)

instance RunMessage RuthTurner where
  runMessage msg e@(RuthTurner attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addToVictory attrs
      pure e
    _ -> RuthTurner <$> liftRunMessage msg attrs
