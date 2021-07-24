module Arkham.Types.Enemy.Cards.MobEnforcer
  ( MobEnforcer(..)
  , mobEnforcer
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.Source
import Arkham.Types.Window

newtype MobEnforcer = MobEnforcer EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mobEnforcer :: EnemyCard MobEnforcer
mobEnforcer = enemyWith
  MobEnforcer
  Cards.mobEnforcer
  (4, Static 3, 3)
  (1, 0)
  (preyL .~ SetToBearer)

instance HasModifiersFor env MobEnforcer

instance ActionRunner env => HasActions env MobEnforcer where
  getActions iid NonFast (MobEnforcer attrs@EnemyAttrs {..}) =
    withBaseActions iid NonFast attrs $ do
      resourceCount <- getResourceCount iid
      locationId <- getId @LocationId iid
      pure
        [ UseAbility
            iid
            (mkAbility
              (toSource attrs)
              1
              (ActionAbility
                (Just Parley)
                (Costs [ActionCost 1, ResourceCost 4])
              )
            )
        | resourceCount >= 4 && locationId == enemyLocation
        ]
  getActions i window (MobEnforcer attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env MobEnforcer where
  runMessage msg e@(MobEnforcer attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility _ (EnemySource eid) _ 1 _ | eid == enemyId ->
      e <$ push (Discard $ toTarget attrs)
    _ -> MobEnforcer <$> runMessage msg attrs
