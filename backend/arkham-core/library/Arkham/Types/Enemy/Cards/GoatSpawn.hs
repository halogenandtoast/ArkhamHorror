{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.GoatSpawn where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Source
import ClassyPrelude
import qualified Data.HashSet as HashSet

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
  runMessage msg (GoatSpawn attrs@Attrs {..}) = case msg of
    EnemyDefeated eid _ _ _ | eid == enemyId -> do
      investigatorIds <- HashSet.toList <$> asks (getSet enemyLocation)
      unshiftMessages
        [ InvestigatorAssignDamage iid (EnemySource eid) 0 1
        | iid <- investigatorIds
        ]
      GoatSpawn <$> runMessage msg attrs
    _ -> GoatSpawn <$> runMessage msg attrs
