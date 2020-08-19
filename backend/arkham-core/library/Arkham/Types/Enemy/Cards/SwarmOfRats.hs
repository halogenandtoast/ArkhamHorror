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

instance (IsInvestigator investigator) => HasActions env investigator SwarmOfRats where
  getActions i (SwarmOfRats attrs) = getActions i attrs

instance (EnemyRunner env) => RunMessage env SwarmOfRats where
  runMessage msg (SwarmOfRats attrs) = SwarmOfRats <$> runMessage msg attrs
