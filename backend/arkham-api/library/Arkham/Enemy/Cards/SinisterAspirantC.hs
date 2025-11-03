module Arkham.Enemy.Cards.SinisterAspirantC (sinisterAspirantC) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype SinisterAspirantC = SinisterAspirantC EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sinisterAspirantC :: EnemyCard SinisterAspirantC
sinisterAspirantC = enemy SinisterAspirantC Cards.sinisterAspirantC (2, Static 3, 4) (0, 1)

instance HasAbilities SinisterAspirantC where
  getAbilities (SinisterAspirantC a) = extend1 a $ restricted a 1 (thisExists a ReadyEnemy) $ forced $ PhaseEnds #when #enemy

instance RunMessage SinisterAspirantC where
  runMessage msg e@(SinisterAspirantC attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> SinisterAspirantC <$> liftRunMessage msg attrs
