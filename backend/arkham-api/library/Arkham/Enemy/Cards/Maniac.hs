module Arkham.Enemy.Cards.Maniac (maniac) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype Maniac = Maniac EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities Maniac where
  getAbilities (Maniac a) = extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)

maniac :: EnemyCard Maniac
maniac = enemy Maniac Cards.maniac (3, Static 4, 1) (1, 0)

instance RunMessage Maniac where
  runMessage msg e@(Maniac attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      nonAttackEnemyDamage (Just iid) (InvestigatorSource iid) 1 attrs
      pure e
    _ -> Maniac <$> liftRunMessage msg attrs
