module Arkham.Enemy.Cards.PennyWhite (pennyWhite) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype PennyWhite = PennyWhite EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pennyWhite :: EnemyCard PennyWhite
pennyWhite = enemy PennyWhite Cards.pennyWhite (4, Static 5, 3) (0, 2)

instance HasAbilities PennyWhite where
  getAbilities (PennyWhite a) =
    extend1 a $ mkAbility a 1 $ forced $ Matcher.EnemyEvaded #after You (be a)

instance RunMessage PennyWhite where
  runMessage msg e@(PennyWhite attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure e
    _ -> PennyWhite <$> liftRunMessage msg attrs
