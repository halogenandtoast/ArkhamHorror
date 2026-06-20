module Arkham.Enemy.Cards.LupineHybridA (lupineHybridA) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Location.Grid
import Arkham.Matcher

newtype LupineHybridA = LupineHybridA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lupineHybridA :: EnemyCard LupineHybridA
lupineHybridA =
  enemyWith
    LupineHybridA
    Cards.lupineHybridA
    (spawnAtL ?~ SpawnAt (LocationInPosition $ Pos 0 2))

instance HasAbilities LupineHybridA where
  getAbilities (LupineHybridA a) =
    extend1 a $ forcedAbility a 1 $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)

instance RunMessage LupineHybridA where
  runMessage msg e@(LupineHybridA attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure e
    _ -> LupineHybridA <$> liftRunMessage msg attrs
