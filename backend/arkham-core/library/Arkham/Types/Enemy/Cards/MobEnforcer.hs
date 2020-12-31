{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Enemy.Cards.MobEnforcer
  ( MobEnforcer(..)
  , mobEnforcer
  )
where

import Arkham.Import

import Arkham.Types.Action
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype MobEnforcer = MobEnforcer Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mobEnforcer :: EnemyId -> MobEnforcer
mobEnforcer uuid = MobEnforcer $ (weaknessBaseAttrs uuid "01101")
  { enemyHealthDamage = 1
  , enemySanityDamage = 0
  , enemyFight = 4
  , enemyHealth = Static 3
  , enemyEvade = 3
  , enemyPrey = SetToBearer
  }

instance HasModifiersFor env MobEnforcer where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MobEnforcer where
  getActions iid NonFast (MobEnforcer attrs@Attrs {..}) =
    withBaseActions iid NonFast attrs $ do
      resourceCount <- getResourceCount iid
      locationId <- getId @LocationId iid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (toSource attrs)
              1
              (ActionAbility (Just Parley) (ActionCost 1))
            )
        | resourceCount >= 4 && locationId == enemyLocation
        ]
  getActions i window (MobEnforcer attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env MobEnforcer where
  runMessage msg e@(MobEnforcer attrs@Attrs {..}) = case msg of
    UseCardAbility iid (EnemySource eid) _ 1 | eid == enemyId ->
      e <$ unshiftMessages [SpendResources iid 4, Discard (EnemyTarget enemyId)]
    _ -> MobEnforcer <$> runMessage msg attrs
