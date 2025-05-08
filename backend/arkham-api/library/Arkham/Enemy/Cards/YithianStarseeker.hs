module Arkham.Enemy.Cards.YithianStarseeker (yithianStarseeker) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype YithianStarseeker = YithianStarseeker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianStarseeker :: EnemyCard YithianStarseeker
yithianStarseeker =
  enemy YithianStarseeker Cards.yithianStarseeker (3, Static 4, 5) (2, 1)
    & setSpawnAt "Another Dimension"

instance HasAbilities YithianStarseeker where
  getAbilities (YithianStarseeker a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyAttacks #when (DiscardWith $ LengthIs $ GreaterThan $ Static 10) AnyEnemyAttack (be a)

instance RunMessage YithianStarseeker where
  runMessage msg e@(YithianStarseeker attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> YithianStarseeker <$> liftRunMessage msg attrs
