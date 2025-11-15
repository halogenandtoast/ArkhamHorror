module Arkham.Enemy.Cards.ExtradimensionalEnemy (extradimensionalEnemy) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype ExtradimensionalEnemy = ExtradimensionalEnemy EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extradimensionalEnemy :: EnemyCard ExtradimensionalEnemy
extradimensionalEnemy = enemy ExtradimensionalEnemy Cards.extradimensionalEnemy (1, Static 1, 1) (1, 1)

instance HasAbilities ExtradimensionalEnemy where
  getAbilities (ExtradimensionalEnemy a) = extend1 a $ mkAbility a 1 $ forced $ EnemyDisengaged #after You (be a)

instance RunMessage ExtradimensionalEnemy where
  runMessage msg e@(ExtradimensionalEnemy attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> ExtradimensionalEnemy <$> liftRunMessage msg attrs
