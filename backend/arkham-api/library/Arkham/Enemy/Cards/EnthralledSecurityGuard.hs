module Arkham.Enemy.Cards.EnthralledSecurityGuard (enthralledSecurityGuard) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks, EnemyEvaded)
import Arkham.Matcher

newtype EnthralledSecurityGuard = EnthralledSecurityGuard EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enthralledSecurityGuard :: EnemyCard EnthralledSecurityGuard
enthralledSecurityGuard = enemy EnthralledSecurityGuard Cards.enthralledSecurityGuard (2, Static 3, 4) (1, 0)

instance HasAbilities EnthralledSecurityGuard where
  getAbilities (EnthralledSecurityGuard a) =
    extend
      a
      [ forcedAbility a 1 $ EnemyAttacks #when You AnyEnemyAttack (be a)
      , forcedAbility a 2 $ EnemyEvaded #after Anyone (be a)
      ]

instance RunMessage EnthralledSecurityGuard where
  runMessage msg e@(EnthralledSecurityGuard attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardTopOfDeck iid (attrs.ability 1) 2
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      defeatEnemy attrs iid (attrs.ability 2)
      pure e
    _ -> EnthralledSecurityGuard <$> liftRunMessage msg attrs
