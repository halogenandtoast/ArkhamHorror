{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.StatsRefresh (
  module Entity.Arkham.StatsRefresh,
) where

import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist.TH
import Entity
import Json
import Orphans ()
import Relude

{- | When a derived dataset was last rebuilt.

One row per dataset, named rather than keyed by anything structural, because what
it records is an operational fact about a materialized view and not a thing the
game knows about. @durationMs@ is kept so the cost of the rebuild is visible to
whoever is deciding how often it should happen.

A panel reading derived numbers has to be able to say how old they are; without
this it would present a week-old count as the current one.
-}
mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamStatsRefresh sql=arkham_stats_refreshes
  Id UUID default=uuid_generate_v4()
  name Text
  refreshedAt UTCTime
  durationMs Int
  UniqueStatsRefreshName name
  deriving Generic Show
|]

instance ToJSON ArkhamStatsRefresh where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamStatsRefresh"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamStatsRefresh"

-- | The name the game-stats materialized view records itself under.
gameStatsRefreshName :: Text
gameStatsRefreshName = "game_stats"
