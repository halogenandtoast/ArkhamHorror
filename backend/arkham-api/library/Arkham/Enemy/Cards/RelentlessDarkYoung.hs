module Arkham.Enemy.Cards.RelentlessDarkYoung (relentlessDarkYoung) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype RelentlessDarkYoung = RelentlessDarkYoung EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relentlessDarkYoung :: EnemyCard RelentlessDarkYoung
relentlessDarkYoung =
  enemy RelentlessDarkYoung Cards.relentlessDarkYoung (4, Static 5, 2) (2, 1)
    & setPrey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)

instance HasAbilities RelentlessDarkYoung where
  getAbilities (RelentlessDarkYoung attrs) = extend1 attrs $ mkAbility attrs 1 $ forced $ RoundEnds #when

instance RunMessage RelentlessDarkYoung where
  runMessage msg e@(RelentlessDarkYoung attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healDamage attrs attrs 2
      pure e
    _ -> RelentlessDarkYoung <$> liftRunMessage msg attrs
