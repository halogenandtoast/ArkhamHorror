{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Maintenance mode: the switch that takes the app off the air so the
database underneath it can be worked on.

A leaf module on purpose. The middleware that enforces the mode lives in
"Foundation", the Redis plumbing that spreads it across pods lives in
"Api.Arkham.Helpers", and that imports "Foundation" -- so anything both of
them need has to sit below the pair of them.
-}
module Api.Arkham.Maintenance (
  Maintenance (..),
  defaultMaintenanceMessage,
  maintenanceKey,
  maintenanceChannel,
) where

import Data.Aeson
import Data.Time.Clock (UTCTime)
import Database.Redis (RedisChannel)
import Relude

-- | The mode, while it is on. Absence is the mode being off.
data Maintenance = Maintenance
  { maintenanceSince :: UTCTime
  {- ^ Set once, when the mode is turned on, and carried unchanged through
  pub/sub and a pod restart so every replica reports the same start time.
  -}
  , maintenanceMessage :: Maybe Text
  {- ^ Shown to players in place of the site. 'defaultMaintenanceMessage'
  when none was given.
  -}
  }
  deriving stock (Show, Generic)

instance ToJSON Maintenance where
  toJSON Maintenance {..} =
    object ["since" .= maintenanceSince, "message" .= maintenanceMessage]

instance FromJSON Maintenance where
  parseJSON = withObject "Maintenance" \o ->
    Maintenance <$> o .: "since" <*> o .:? "message"

defaultMaintenanceMessage :: Text
defaultMaintenanceMessage =
  "Arkham Horror is down for scheduled database maintenance. Games in progress are saved and will be exactly where you left them."

{- | Where the mode is stored, so that a pod starting up mid-window comes up in
maintenance instead of cheerfully serving traffic into a locked table.

No TTL. A TTL would look like a safety net and behave like a hazard: it would
expire mid-@VACUUM FULL@ and send six replicas' worth of queries into an
@ACCESS EXCLUSIVE@ lock, where they would pile up until the pool was gone.
Maintenance left on too long is a visible, fixable mistake; maintenance that
lifts itself halfway through is an outage.
-}
maintenanceKey :: ByteString
maintenanceKey = "arkham:maintenance"

{- | Announces a change to the other pods. Registered as an INITIAL
subscription on the 'Database.Redis.PubSubController' alongside
'Api.Arkham.Helpers.pubSubHealthChannel', so @pubSubForever@ restores it on
every reconnect -- a pod that quietly lost this subscription would keep
serving traffic through a maintenance window.
-}
maintenanceChannel :: RedisChannel
maintenanceChannel = "arkham:maintenance:changed"
