module Arkham.Scenarios.TheDepthsOfYoth.Helpers where

import Arkham.Prelude

import Arkham.Helpers.Log
import Arkham.Classes.Query
import Arkham.Enemy.Types
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Types ( Field (..) )
import Arkham.ScenarioLogKey
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Data.Aeson ( Result (..) )

newtype DepthsOfYothMeta = DepthsOfYothMeta
  { depthLocation :: LocationId
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

incrementDepth :: (Monad m, HasGame m) => m [Message]
incrementDepth = do
  addingToCurrentDepth <- checkWindows
    [Window Timing.When Window.AddingToCurrentDepth]
  pure [addingToCurrentDepth, ScenarioCountIncrementBy CurrentDepth 1]

getCurrentDepth :: (HasGame m, Monad m) => m Int
getCurrentDepth = scenarioCount CurrentDepth

getDepthStart :: (HasGame m, Monad m) => m LocationId
getDepthStart = depthLocation <$> getMeta

getMeta :: (HasGame m, Monad m) => m DepthsOfYothMeta
getMeta = do
  v <- scenarioField ScenarioMeta
  pure $ case fromJSON v of
    Error _ -> error "invalid meta for depths of yoth"
    Success a -> a

toMeta :: LocationId -> Value
toMeta lid = toJSON $ DepthsOfYothMeta lid

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
