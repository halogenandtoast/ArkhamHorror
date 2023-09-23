module Arkham.Enemy.Cards.MobEnforcer (
  MobEnforcer (..),
  mobEnforcer,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message

newtype MobEnforcer = MobEnforcer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mobEnforcer :: EnemyCard MobEnforcer
mobEnforcer =
  enemyWith MobEnforcer Cards.mobEnforcer (4, Static 3, 3) (1, 0)
    $ \a -> a & preyL .~ BearerOf (toId a)

instance HasAbilities MobEnforcer where
  getAbilities (MobEnforcer attrs) =
    withBaseAbilities attrs [restrictedAbility attrs 1 OnSameLocation $ parleyAction (ResourceCost 4)]

instance RunMessage MobEnforcer where
  runMessage msg e@(MobEnforcer attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure e
    _ -> MobEnforcer <$> runMessage msg attrs
