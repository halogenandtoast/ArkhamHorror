module Arkham.Enemy.Creation where

import Arkham.Prelude

import Arkham.Card
import Arkham.Id
import Arkham.Matcher
import Arkham.Placement
import Arkham.Target

data EnemyCreationMethod
  = SpawnEngagedWith InvestigatorId
  | SpawnAtLocation LocationId
  | SpawnWithPlacement Placement
  | SpawnAtLocationMatching LocationMatcher
  | SpawnEngagedWithPrey
  | SpawnViaSpawnInstruction
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

class IsEnemyCreationMethod a where
  toEnemyCreationMethod :: a -> EnemyCreationMethod

instance IsEnemyCreationMethod EnemyCreationMethod where
  toEnemyCreationMethod = id

instance IsEnemyCreationMethod InvestigatorId where
  toEnemyCreationMethod = SpawnEngagedWith

instance IsEnemyCreationMethod LocationId where
  toEnemyCreationMethod = SpawnAtLocation

instance IsEnemyCreationMethod Placement where
  toEnemyCreationMethod = SpawnWithPlacement

instance IsEnemyCreationMethod LocationMatcher where
  toEnemyCreationMethod = SpawnAtLocationMatching

data EnemyCreation msg = MkEnemyCreation
  { enemyCreationCard :: Card
  , enemyCreationEnemyId :: EnemyId
  , enemyCreationMethod :: EnemyCreationMethod
  , enemyCreationTarget :: Maybe Target
  , enemyCreationExhausted :: Bool
  , enemyCreationAfter :: [msg]
  , enemyCreationInvestigator :: Maybe InvestigatorId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
