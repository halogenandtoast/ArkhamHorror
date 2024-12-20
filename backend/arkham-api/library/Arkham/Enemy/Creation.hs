module Arkham.Enemy.Creation where

import Arkham.Prelude

import Arkham.Card
import Arkham.Id
import Arkham.Matcher
import Arkham.Placement
import Arkham.Target
import GHC.Records

data EnemyCreationMethod
  = SpawnEngagedWith InvestigatorId
  | SpawnAtLocation LocationId
  | SpawnWithPlacement Placement
  | SpawnAtLocationMatching LocationMatcher
  | SpawnEngagedWithPrey
  | SpawnViaSpawnInstruction
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

class IsEnemyCreationMethod a where
  toEnemyCreationMethod :: a -> EnemyCreationMethod

instance IsEnemyCreationMethod EnemyCreationMethod where
  toEnemyCreationMethod = id

instance IsEnemyCreationMethod () where
  toEnemyCreationMethod _ = SpawnViaSpawnInstruction

instance IsEnemyCreationMethod InvestigatorId where
  toEnemyCreationMethod = SpawnEngagedWith

instance IsEnemyCreationMethod LocationId where
  toEnemyCreationMethod = SpawnAtLocation

instance IsEnemyCreationMethod Placement where
  toEnemyCreationMethod = SpawnWithPlacement

instance IsEnemyCreationMethod LocationMatcher where
  toEnemyCreationMethod = SpawnAtLocationMatching

instance IsEnemyCreationMethod CardDef where
  toEnemyCreationMethod = SpawnAtLocationMatching . locationIs

data EnemyCreation msg = MkEnemyCreation
  { enemyCreationCard :: Card
  , enemyCreationEnemyId :: EnemyId
  , enemyCreationMethod :: EnemyCreationMethod
  , enemyCreationTarget :: Maybe Target
  , enemyCreationExhausted :: Bool
  , enemyCreationBefore :: [msg]
  , enemyCreationAfter :: [msg]
  , enemyCreationInvestigator :: Maybe InvestigatorId
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass ToJSON

instance FromJSON msg => FromJSON (EnemyCreation msg) where
  parseJSON = withObject "EnemyCreation" \o -> do
    enemyCreationCard <- o .: "enemyCreationCard"
    enemyCreationEnemyId <- o .: "enemyCreationEnemyId"
    enemyCreationMethod <- o .: "enemyCreationMethod"
    enemyCreationTarget <- o .: "enemyCreationTarget"
    enemyCreationExhausted <- o .: "enemyCreationExhausted"
    enemyCreationBefore <- o .:? "enemyCreationBefore" .!= []
    enemyCreationAfter <- o .: "enemyCreationAfter"
    enemyCreationInvestigator <- o .: "enemyCreationInvestigator"
    pure $ MkEnemyCreation {..}

createExhausted :: EnemyCreation msg -> EnemyCreation msg
createExhausted ec = ec {enemyCreationExhausted = True}

instance HasField "enemy" (EnemyCreation msg) EnemyId where
  getField = enemyCreationEnemyId
