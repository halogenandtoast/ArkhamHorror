module Arkham.Enemy.Cards.LupineHybridC (lupineHybridC) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Location.Grid
import Arkham.Matcher

newtype LupineHybridC = LupineHybridC EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lupineHybridC :: EnemyCard LupineHybridC
lupineHybridC =
  enemyWith
    LupineHybridC
    Cards.lupineHybridC
    (spawnAtL ?~ SpawnAt (LocationInPosition $ Pos 0 (-2)))

instance HasAbilities LupineHybridC where
  getAbilities (LupineHybridC a) =
    extend1 a $ forcedAbility a 1 $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)

instance RunMessage LupineHybridC where
  runMessage msg e@(LupineHybridC attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure e
    _ -> LupineHybridC <$> liftRunMessage msg attrs
