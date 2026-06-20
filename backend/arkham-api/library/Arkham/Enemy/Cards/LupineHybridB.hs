module Arkham.Enemy.Cards.LupineHybridB (lupineHybridB) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Location.Grid
import Arkham.Matcher

newtype LupineHybridB = LupineHybridB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lupineHybridB :: EnemyCard LupineHybridB
lupineHybridB =
  enemyWith
    LupineHybridB
    Cards.lupineHybridB
    (spawnAtL ?~ SpawnAt (LocationInPosition $ Pos 2 0))

instance HasAbilities LupineHybridB where
  getAbilities (LupineHybridB a) =
    extend1 a $ forcedAbility a 1 $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)

instance RunMessage LupineHybridB where
  runMessage msg e@(LupineHybridB attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure e
    _ -> LupineHybridB <$> liftRunMessage msg attrs
