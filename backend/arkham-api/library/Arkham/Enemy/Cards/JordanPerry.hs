module Arkham.Enemy.Cards.JordanPerry (jordanPerry) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype JordanPerry = JordanPerry EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jordanPerry :: EnemyCard JordanPerry
jordanPerry = enemy JordanPerry Cards.jordanPerry (2, Static 8, 2) (1, 1)

instance HasAbilities JordanPerry where
  getAbilities (JordanPerry a) =
    extend1 a
      $ restricted a 1 (EnemyCriteria $ ThisEnemy EnemyWithAnyDamage)
      $ forced
      $ PhaseBegins #when #enemy

instance RunMessage JordanPerry where
  runMessage msg e@(JordanPerry attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure e
    _ -> JordanPerry <$> liftRunMessage msg attrs
