{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Spawn where

import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Spawn (SpawnDetails)
import Arkham.Zone (OutOfPlayZone)
import Data.Aeson.TH

-- | Messages dealing with enemy spawn and location-entry lifecycle.
--
-- The wrapper type is named 'SpawnMessage' (no "Enemy" prefix) to avoid
-- collision with 'Arkham.Message.Type.EnemySpawnMessage'.
data SpawnMessage
  = EnemySpawn_ SpawnDetails
  | EnemySpawned_ SpawnDetails
  | EnemySpawnAtLocationMatching_ (Maybe InvestigatorId) LocationMatcher EnemyId
  | EnemySpawnFromOutOfPlay_ OutOfPlayZone (Maybe InvestigatorId) LocationId EnemyId
  | EnemySpawnEngagedWithPrey_ EnemyId
  | EnemySpawnEngagedWith_ EnemyId InvestigatorMatcher
  | EnemyEntered_ EnemyId LocationId
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''SpawnMessage)
