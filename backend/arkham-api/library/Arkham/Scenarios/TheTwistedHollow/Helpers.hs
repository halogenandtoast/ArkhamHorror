module Arkham.Scenarios.TheTwistedHollow.Helpers where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Types (Enemy, Field (..))
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Tracing
import Arkham.Zone

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theTwistedHollow" a

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
