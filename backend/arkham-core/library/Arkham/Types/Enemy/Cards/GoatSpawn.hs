{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.GoatSpawn where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import ClassyPrelude

newtype GoatSpawn = GoatSpawn Attrs
  deriving newtype (Show, ToJSON, FromJSON)

goatSpawn :: EnemyId -> GoatSpawn
goatSpawn uuid = GoatSpawn $ (baseAttrs uuid "01180")
  { enemyHealthDamage = 1
  , enemyFight = 3
  , enemyHealth = Static 3
  , enemyEvade = 2
  }

instance (IsInvestigator investigator) => HasActions env investigator GoatSpawn where
  getActions i window (GoatSpawn attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GoatSpawn where
  runMessage msg (GoatSpawn attrs) = GoatSpawn <$> runMessage msg attrs
