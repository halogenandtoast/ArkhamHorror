module Arkham.Enemy.Cards.SinisterAspirantB (sinisterAspirantB) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype SinisterAspirantB = SinisterAspirantB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sinisterAspirantB :: EnemyCard SinisterAspirantB
sinisterAspirantB = enemy SinisterAspirantB Cards.sinisterAspirantB (2, Static 3, 4) (0, 1)

instance HasAbilities SinisterAspirantB where
  getAbilities (SinisterAspirantB a) = extend1 a $ restricted a 1 (thisExists a ReadyEnemy) $ forced $ PhaseEnds #after #enemy

instance RunMessage SinisterAspirantB where
  runMessage msg e@(SinisterAspirantB attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> SinisterAspirantB <$> liftRunMessage msg attrs
