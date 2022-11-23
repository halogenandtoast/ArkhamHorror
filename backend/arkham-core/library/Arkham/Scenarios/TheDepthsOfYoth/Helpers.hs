module Arkham.Scenarios.TheDepthsOfYoth.Helpers where

import Arkham.Prelude

import Arkham.Classes.Query
import Arkham.Enemy.Types
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Types ( Field (..) )
import Data.Aeson ( Result (..) )
import Arkham.Window qualified as Window

data DepthsOfYothMeta = DepthsOfYothMeta
  { currentDepth :: Int
  , depthLocation :: LocationId
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

incrementDepth :: (HasGame m) => m [Message]
incrementDepth = do
  addingToCurrentDepth <- checkWindows
    [Window Timing.When Window.AddingToCurrentDepth]
  depth <- (+ 1) <$> getCurrentDepth
  start <- getDepthStart
  pure [addingToCurrentDepth, SetScenarioMeta $ toMeta depth start]

getCurrentDepth :: (HasGame m, Monad m) => m Int
getCurrentDepth = currentDepth <$> getMeta

getDepthStart :: (HasGame m, Monad m) => m LocationId
getDepthStart = depthLocation <$> getMeta

getMeta :: (HasGame m, Monad m) => m DepthsOfYothMeta
getMeta = do
  v <- scenarioField ScenarioMeta
  pure $ case fromJSON v of
    Error _ -> error "invalid meta for depths of yoth"
    Success a -> a

toMeta :: Int -> LocationId -> Value
toMeta n lid = toJSON $ DepthsOfYothMeta n lid

getInPursuitEnemyWithHighestEvade
  :: (HasGame m, Monad m) => m (HashSet EnemyId)
getInPursuitEnemyWithHighestEvade = do
  inPursuit <- getInPursuitEnemies
  evadeValue <- getMax0 <$> selectAgg
    (Max . fromMaybe 0)
    (SetAsideEnemyField EnemyEvade)
    (SetAsideMatcher $ EnemyOneOf $ map EnemyWithId $ toList inPursuit)
  setFromList <$> filterM
    (fieldMap (SetAsideEnemyField EnemyEvade) ((== Just evadeValue)))
    (toList inPursuit)

getInPursuitEnemies :: (HasGame m, Monad m) => m (HashSet EnemyId)
getInPursuitEnemies = do
  enemies <- select $ SetAsideMatcher AnyEnemy
  setFromList <$> filterM
    (fieldMap (SetAsideEnemyField EnemyPlacement) (== Pursuit))
    (toList enemies)

getPlacePursuitEnemyMessages :: (HasGame m, Monad m) => m [Message]
getPlacePursuitEnemyMessages = do
  choices <- toList <$> getInPursuitEnemyWithHighestEvade
  lead <- getLeadInvestigatorId
  depthStart <- getDepthStart
  pure $ do
    guard $ notNull choices
    pure $ chooseOrRunOne
      lead
      [ targetLabel choice [PlaceEnemy choice $ AtLocation depthStart]
      | choice <- choices
      ]
