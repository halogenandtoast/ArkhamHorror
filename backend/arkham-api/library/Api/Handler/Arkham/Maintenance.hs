{-# LANGUAGE OverloadedRecordDot #-}

{- | The maintenance switch.

Everything here is deliberately independent of Postgres. The whole point of the
endpoint is to be usable while the database is unavailable -- during a
@VACUUM FULL@, a failover, a disk emergency -- so a single @SELECT@ anywhere on
this path would make it the first thing to block behind whatever it was meant
to protect you from.
-}
module Api.Handler.Arkham.Maintenance (
  getApiV1MaintenanceR,
  postApiV1MaintenanceR,
  deleteApiV1MaintenanceR,
) where

import Api.Arkham.Helpers
import Api.Arkham.Maintenance
import Auth.ApiKey (digestOf)
import Control.Concurrent.MVar qualified as MVar
import Data.Aeson (Result (..))
import Data.Map.Strict qualified as Map
import Data.Time.Clock (getCurrentTime)
import Import

newtype MaintenanceRequest = MaintenanceRequest
  { message :: Maybe Text
  }
  deriving stock Generic
  deriving anyclass FromJSON

{- | Gate every method on the shared secret.

Compared as SHA-256 digests rather than as text: equal-length hex strings, so
'==' gives nothing away about how much of a wrong token was right. Reuses
'Auth.ApiKey.digestOf' because an API key is compared the same way.

An unset token 404s rather than 403s. The endpoint should not exist on a
deployment that has not configured it -- development, the test suite, anyone's
local checkout -- and a 404 says that honestly instead of advertising a switch
and then refusing it.
-}
requireMaintenanceToken :: Handler ()
requireMaintenanceToken = do
  mExpected <- getsApp (appMaintenanceToken . appSettings)
  expected <- maybe notFound pure mExpected
  presented <- maybe "" decodeUtf8 <$> lookupHeader "X-Maintenance-Token"
  unless (digestOf presented == digestOf expected)
    $ permissionDenied "Bad or missing X-Maintenance-Token"

-- GET /api/v1/maintenance -- is it on, and is anyone still connected?
getApiV1MaintenanceR :: Handler Value
getApiV1MaintenanceR = do
  requireMaintenanceToken
  mMode <- getsApp appMaintenance >>= readTVarIO
  rooms <- getsApp appGameRooms >>= liftIO . MVar.readMVar
  clients <- sum <$> traverse roomClientCount (Map.elems rooms)
  pure
    $ object
      [ "maintenance" .= isJust mMode
      , "since" .= (maintenanceSince <$> mMode)
      , "message" .= (maintenanceMessage =<< mMode)
      , "gameRooms" .= Map.size rooms
      , "connectedClients" .= clients
      , "note"
          .= ( "Room and client counts are for THIS pod only, and the app being quiet is not \
               \the database being quiet. Check pg_stat_activity before taking any lock."
                 :: Text
             )
      ]

{- | POST /api/v1/maintenance -- turn it on.

The message can come as @?message=@ or as a JSON body; a missing or unparseable
body is not an error, because the common case is a bare @curl -X POST@ and
failing that call would mean the switch did not throw.

'maintenanceSince' is stamped here, once, so every replica and every restart
afterwards reports the same start time.
-}
postApiV1MaintenanceR :: Handler Value
postApiV1MaintenanceR = do
  requireMaintenanceToken
  fromQuery <- lookupGetParam "message"
  fromBody <-
    (parseCheckJsonBody :: Handler (Result MaintenanceRequest)) >>= \case
      Success req -> pure req.message
      Error _ -> pure Nothing
  now <- liftIO getCurrentTime
  let mode = Maintenance now (fromQuery <|> fromBody)
  setMaintenance (Just mode)
  pure
    $ object
      [ "maintenance" .= True
      , "since" .= maintenanceSince mode
      , "message" .= fromMaybe defaultMaintenanceMessage (maintenanceMessage mode)
      , "next"
          .= ( "Sockets are closed and new requests get a 503. In-flight actions were \
               \allowed to finish, so wait for pg_stat_activity to go quiet -- the \
               \connection pool also drops idle connections after 60s -- before taking \
               \an exclusive lock."
                 :: Text
             )
      ]

-- DELETE /api/v1/maintenance -- turn it off.
deleteApiV1MaintenanceR :: Handler Value
deleteApiV1MaintenanceR = do
  requireMaintenanceToken
  setMaintenance Nothing
  pure $ object ["maintenance" .= False]
