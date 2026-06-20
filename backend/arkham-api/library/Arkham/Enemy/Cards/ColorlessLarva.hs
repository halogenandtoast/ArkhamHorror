module Arkham.Enemy.Cards.ColorlessLarva (colorlessLarva) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype ColorlessLarva = ColorlessLarva EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

colorlessLarva :: EnemyCard ColorlessLarva
colorlessLarva = enemy ColorlessLarva Cards.colorlessLarva

instance HasAbilities ColorlessLarva where
  getAbilities (ColorlessLarva a) =
    extend1 a $ forcedAbility a 1 $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)

instance RunMessage ColorlessLarva where
  runMessage msg e@(ColorlessLarva attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      defeatEnemy attrs iid (attrs.ability 1)
      pure e
    _ -> ColorlessLarva <$> liftRunMessage msg attrs
