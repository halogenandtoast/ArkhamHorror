module Arkham.Enemy.Cards.EnthralledSecurityGuard (
  enthralledSecurityGuard,
  EnthralledSecurityGuard (..),
)
where

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
      , forcedAbility a 2 $ EnemyEvaded #after You (be a)
      ]

instance RunMessage EnthralledSecurityGuard where
  runMessage msg e@(EnthralledSecurityGuard attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DiscardTopOfDeck iid 2 (attrs.ability 1) Nothing
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      defeatEnemy attrs.id iid (attrs.ability 2)
      pure e
    _ -> EnthralledSecurityGuard <$> liftRunMessage msg attrs
