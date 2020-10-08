{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.SwarmOfRats where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import ClassyPrelude

newtype SwarmOfRats = SwarmOfRats Attrs
  deriving newtype (Show, ToJSON, FromJSON)

swarmOfRats :: EnemyId -> SwarmOfRats
swarmOfRats uuid = SwarmOfRats
  $ (baseAttrs uuid "01159") { enemyHealthDamage = 1, enemyEvade = 3 }

instance HasModifiersFor env investigator SwarmOfRats where
  getModifiersFor _ _ _ = pure []

instance HasModifiers env SwarmOfRats where
  getModifiers _ (SwarmOfRats Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance (IsInvestigator investigator) => HasActions env investigator SwarmOfRats where
  getActions i window (SwarmOfRats attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env SwarmOfRats where
  runMessage msg (SwarmOfRats attrs) = SwarmOfRats <$> runMessage msg attrs
