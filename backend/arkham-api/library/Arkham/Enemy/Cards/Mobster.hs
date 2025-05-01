module Arkham.Enemy.Cards.Mobster (mobster) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype Mobster = Mobster EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mobster :: EnemyCard Mobster
mobster = enemy Mobster Cards.mobster (2, Static 2, 2) (1, 0)

instance HasAbilities Mobster where
  getAbilities (Mobster x) =
    extend1 x $ mkAbility x 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be x)

instance RunMessage Mobster where
  runMessage msg e@(Mobster attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseResources iid (attrs.ability 1) 1
      pure e
    _ -> Mobster <$> liftRunMessage msg attrs
