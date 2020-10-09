{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.MobEnforcer where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window
import ClassyPrelude

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

instance HasModifiersFor env investigator MobEnforcer where
  getModifiersFor _ _ _ = pure []

instance HasModifiers env MobEnforcer where
  getModifiers _ (MobEnforcer Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance (IsInvestigator investigator) => HasActions env investigator MobEnforcer where
  getActions i NonFast (MobEnforcer attrs@Attrs {..}) = do
    baseActions <- getActions i NonFast attrs
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             (getId () i)
             (mkAbility (EnemySource enemyId) 1 (ActionAbility 1 (Just Parley)))
         | resourceCount i >= 4 && locationOf i == enemyLocation
         ]
  getActions i window (MobEnforcer attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env MobEnforcer where
  runMessage msg e@(MobEnforcer attrs@Attrs {..}) = case msg of
    UseCardAbility iid (EnemySource eid) _ 1 | eid == enemyId ->
      e <$ unshiftMessages [SpendResources iid 4, Discard (EnemyTarget enemyId)]
    _ -> MobEnforcer <$> runMessage msg attrs
