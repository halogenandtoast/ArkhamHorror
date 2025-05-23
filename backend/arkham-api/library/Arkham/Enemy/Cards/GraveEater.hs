module Arkham.Enemy.Cards.GraveEater (graveEater) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher

newtype GraveEater = GraveEater EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveEater :: EnemyCard GraveEater
graveEater = enemy GraveEater Cards.graveEater (2, Static 2, 2) (1, 1)

instance HasAbilities GraveEater where
  getAbilities (GraveEater x) =
    extend1 x $ mkAbility x 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be x)

instance RunMessage GraveEater where
  runMessage msg e@(GraveEater attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscard iid attrs
      pure e
    _ -> GraveEater <$> liftRunMessage msg attrs
