module Arkham.Scenarios.TheDepthsOfYoth.Helpers where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Enemy.Types
import Arkham.Helpers.Log
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message (Message (ScenarioCountIncrementBy))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Source
import Arkham.Window qualified as Window
import Arkham.Zone
import Data.Aeson (Result (..))

data DepthsOfYothMeta = DepthsOfYothMeta
  { depthLocation :: LocationId
  , currentExploreSource :: Maybe Source
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

incrementDepth :: ReverseQueue m => m ()
incrementDepth = do
  checkWhen Window.AddingToCurrentDepth
  push $ ScenarioCountIncrementBy CurrentDepth 1

getCurrentDepth :: HasGame m => m Int
getCurrentDepth = scenarioCount CurrentDepth

getDepthStart :: HasGame m => m LocationId
getDepthStart = depthLocation <$> getMeta

getCurrentExploreSource :: HasGame m => m (Maybe Source)
getCurrentExploreSource = currentExploreSource <$> getMeta

getMeta :: HasGame m => m DepthsOfYothMeta
getMeta = do
  v <- scenarioField ScenarioMeta
  case fromJSON v of
    Error _ -> error "invalid meta for depths of yoth"
    Success a -> pure a

toMeta :: LocationId -> Maybe Source -> Value
toMeta lid msource = toJSON $ DepthsOfYothMeta lid msource

getInPursuitEnemyWithHighestEvade
  :: HasGame m => m (Set EnemyId)
getInPursuitEnemyWithHighestEvade = do
  inPursuit <- getInPursuitEnemies
  evadeValue <-
    selectAgg' @(OutOfPlayEntity 'PursuitZone Enemy)
      (Max0 . fromMaybe 0)
      (OutOfPlayEnemyField PursuitZone EnemyEvade)
      (OutOfPlayEnemy PursuitZone $ EnemyOneOf $ map EnemyWithId $ toList inPursuit)
  setFromList
    <$> filterM
      ( fieldMap @(OutOfPlayEntity 'PursuitZone Enemy)
          (OutOfPlayEnemyField PursuitZone EnemyEvade)
          (== Just evadeValue)
      )
      (toList inPursuit)

getInPursuitEnemies :: HasGame m => m [EnemyId]
getInPursuitEnemies = select $ OutOfPlayEnemy PursuitZone AnyEnemy

placePursuitEnemies :: ReverseQueue m => m ()
placePursuitEnemies = do
  choices <- toList <$> getInPursuitEnemyWithHighestEvade
  depthStart <- getDepthStart
  leadChooseOrRunOneM $ targets choices \choice -> place choice $ AtLocation depthStart

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theDepthsOfYoth" a
