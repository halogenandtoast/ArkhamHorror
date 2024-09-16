module Arkham.Enemy.Cards.LurkingDeepOne (lurkingDeepOne, LurkingDeepOne (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype LurkingDeepOne = LurkingDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lurkingDeepOne :: EnemyCard LurkingDeepOne
lurkingDeepOne =
  enemyWith LurkingDeepOne Cards.lurkingDeepOne (2, Static 2, 4) (1, 1)
    $ preyL
    .~ Prey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)

instance HasAbilities LurkingDeepOne where
  getAbilities (LurkingDeepOne a) = extend a [forcedAbility a 1 $ EnemyEngaged #after You (be a)]

instance RunMessage LurkingDeepOne where
  runMessage msg e@(LurkingDeepOne attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid attrs 1
      pure e
    _ -> LurkingDeepOne <$> liftRunMessage msg attrs
