{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.MobEnforcer where

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
  getModifiersFor _ _ _ = pure []

instance HasModifiers env MobEnforcer where
  getModifiers _ (MobEnforcer Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env MobEnforcer where
  getActions iid NonFast (MobEnforcer attrs@Attrs {..}) = do
    baseActions <- getActions iid NonFast attrs
    resourceCount <- getResourceCount iid
    locationId <- asks $ getId @LocationId iid
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             iid
             (mkAbility (EnemySource enemyId) 1 (ActionAbility 1 (Just Parley)))
         | resourceCount >= 4 && locationId == enemyLocation
         ]
  getActions i window (MobEnforcer attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env MobEnforcer where
  runMessage msg e@(MobEnforcer attrs@Attrs {..}) = case msg of
    UseCardAbility iid (EnemySource eid) _ 1 | eid == enemyId ->
      e <$ unshiftMessages [SpendResources iid 4, Discard (EnemyTarget enemyId)]
    _ -> MobEnforcer <$> runMessage msg attrs
