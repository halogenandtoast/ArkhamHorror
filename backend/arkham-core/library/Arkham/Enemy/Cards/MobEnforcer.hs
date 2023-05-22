module Arkham.Enemy.Cards.MobEnforcer
  ( MobEnforcer(..)
  , mobEnforcer
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message

newtype MobEnforcer = MobEnforcer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mobEnforcer :: EnemyCard MobEnforcer
mobEnforcer = enemyWith
  MobEnforcer
  Cards.mobEnforcer
  (4, Static 3, 3)
  (1, 0)
  (\a -> a & preyL .~ BearerOf (toId a))

instance HasAbilities MobEnforcer where
  getAbilities (MobEnforcer attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 OnSameLocation
        $ ActionAbility (Just Parley) (Costs [ActionCost 1, ResourceCost 4])
    ]

instance RunMessage MobEnforcer where
  runMessage msg e@(MobEnforcer attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility _ (EnemySource eid) 1 _ _ | eid == enemyId ->
      e <$ push (Discard (toAbilitySource attrs 1) $ toTarget attrs)
    _ -> MobEnforcer <$> runMessage msg attrs
