module Arkham.Enemy.Spawn where

import Arkham.Prelude

import Arkham.Card
import Arkham.Id
import Arkham.Placement
import Arkham.Matcher

data SpawnMethod
  = SpawnEngagedWith InvestigatorId
  | SpawnAtLocation LocationId
  | SpawnWithPlacement Placement
  | SpawnAtLocationMatching LocationMatcher
  | SpawnEngagedWithPrey
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SpawnEnemy msg = MkSpawnEnemy
  { spawnCard :: Card
  , spawnEnemyId :: EnemyId
  , spawnMethod :: SpawnMethod
  , spawnAfter :: [msg]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- createEnemy :: MonadRandom m => Card -> m (EnemyId, Message)
-- createEnemy c = do
--   enemyId <- getRandom
--   pure (enemyId, CreateEnemy enemyId c)

-- createEnemy_ :: MonadRandom m => Card -> m Message
-- createEnemy_ = fmap snd . createEnemy

-- createEnemyWithPlacement :: MonadRandom m => Card -> Placement -> m (EnemyId, Message)
-- createEnemyWithPlacement c placement = do
--   enemyId <- getRandom
--   pure (enemyId, CreateEnemyWithPlacement enemyId c placement)

-- createEnemyWithPlacement_ :: MonadRandom m => Card -> Placement -> m Message
-- createEnemyWithPlacement_ c placement = snd <$> createEnemyWithPlacement c placement

-- createEnemyAt :: MonadRandom m => Card -> LocationId -> Maybe Target -> m (EnemyId, Message)
-- createEnemyAt c lid mTarget = do
--   enemyId <- getRandom
--   pure (enemyId, CreateEnemyAt enemyId c lid mTarget)

-- createEnemyAt_ :: MonadRandom m => Card -> LocationId -> Maybe Target -> m Message
-- createEnemyAt_ c lid mTarget = snd <$> createEnemyAt c lid mTarget

-- createEnemyAtLocationMatching :: MonadRandom m => Card -> LocationMatcher -> m (EnemyId, Message)
-- createEnemyAtLocationMatching c matcher = do
--   enemyId <- getRandom
--   pure (enemyId, CreateEnemyAtLocationMatching enemyId c matcher)

-- createEnemyAtLocationMatching_ :: MonadRandom m => Card -> LocationMatcher -> m Message
-- createEnemyAtLocationMatching_ c matcher = snd <$> createEnemyAtLocationMatching c matcher

-- createEnemyEngagedWithPrey :: MonadRandom m => Card -> m (EnemyId, Message)
-- createEnemyEngagedWithPrey c = do
--   enemyId <- getRandom
--   pure (enemyId, CreateEnemyEngagedWithPrey enemyId c)

-- createEnemyEngagedWithPrey_ :: MonadRandom m => Card -> m Message
-- createEnemyEngagedWithPrey_ = fmap snd . createEnemyEngagedWithPrey
