module Arkham.Enemy.Cards.SinisterAspirantA (sinisterAspirantA) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype SinisterAspirantA = SinisterAspirantA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sinisterAspirantA :: EnemyCard SinisterAspirantA
sinisterAspirantA = enemy SinisterAspirantA Cards.sinisterAspirantA (2, Static 3, 4) (0, 1)

instance HasAbilities SinisterAspirantA where
  getAbilities (SinisterAspirantA a) = extend1 a $ restricted a 1 (thisExists a ReadyEnemy) $ forced $ PhaseEnds #when #enemy

instance RunMessage SinisterAspirantA where
  runMessage msg e@(SinisterAspirantA attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> SinisterAspirantA <$> liftRunMessage msg attrs
