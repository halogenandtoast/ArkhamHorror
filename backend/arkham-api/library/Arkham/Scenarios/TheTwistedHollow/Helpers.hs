module Arkham.Scenarios.TheTwistedHollow.Helpers where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Enemy.Types (Enemy, Field (..))
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher hiding (EnemyDrawnFrom)
import Arkham.Message (Message (..))
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.Token
import Arkham.Tracing
import Arkham.Zone

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theTwistedHollow" a

getDarknessLevel :: (HasGame m, Tracing m) => m Int
getDarknessLevel = scenarioFieldMap ScenarioTokens (countTokens DarknessLevel)

pursuitEnemiesWithHighestFight :: (HasGame m, Tracing m) => m [EnemyId]
pursuitEnemiesWithHighestFight = do
  enemies <- select $ OutOfPlayEnemy PursuitZone EnemyWithFight
  fightValue <-
    selectAgg' @(OutOfPlayEntity 'PursuitZone Enemy)
      (Max0 . fromMaybe 0)
      (OutOfPlayEnemyField PursuitZone EnemyFight)
      (OutOfPlayEnemy PursuitZone EnemyWithFight)
  filterM
    ( fieldMap @(OutOfPlayEntity 'PursuitZone Enemy)
        (OutOfPlayEnemyField PursuitZone EnemyFight)
        (== Just fightValue)
    )
    enemies

pursuitEnemiesWithHighestEvade :: (HasGame m, Tracing m) => m [EnemyId]
pursuitEnemiesWithHighestEvade = do
  enemies <- select $ OutOfPlayEnemy PursuitZone EnemyWithEvade
  evadeValue <-
    selectAgg' @(OutOfPlayEntity 'PursuitZone Enemy)
      (Max0 . fromMaybe 0)
      (OutOfPlayEnemyField PursuitZone EnemyEvade)
      (OutOfPlayEnemy PursuitZone EnemyWithEvade)
  filterM
    ( fieldMap @(OutOfPlayEntity 'PursuitZone Enemy)
        (OutOfPlayEnemyField PursuitZone EnemyEvade)
        (== Just evadeValue)
    )
    enemies

pursuitEnemiesWithLowestEvade :: (HasGame m, Tracing m) => m [EnemyId]
pursuitEnemiesWithLowestEvade = do
  enemies <- select $ OutOfPlayEnemy PursuitZone EnemyWithEvade
  evadeValue <-
    selectAgg' @(OutOfPlayEntity 'PursuitZone Enemy)
      (Min . fromMaybe 0)
      (OutOfPlayEnemyField PursuitZone EnemyEvade)
      (OutOfPlayEnemy PursuitZone EnemyWithEvade)
  filterM
    ( fieldMap @(OutOfPlayEntity 'PursuitZone Enemy)
        (OutOfPlayEnemyField PursuitZone EnemyEvade)
        (== Just evadeValue)
    )
    enemies

drawEnemyFromPursuit :: ReverseQueue m => InvestigatorId -> EnemyId -> m ()
drawEnemyFromPursuit iid eid = do
  push $ UpdateEnemy eid $ Update EnemyDrawnFrom Nothing
  push $ InvestigatorDrawEnemy iid eid
