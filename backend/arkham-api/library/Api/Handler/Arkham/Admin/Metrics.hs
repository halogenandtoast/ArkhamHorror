module Api.Handler.Arkham.Admin.Metrics (
  getApiV1AdminMetricsR,
  deleteApiV1AdminMetricsR,
) where

import Arkham.Metrics qualified as Metrics
import Data.Aeson qualified as Aeson
import Import

-- GET /api/v1/admin/metrics
--
-- Returns either a "metrics not enabled" stub or a JSON document describing
-- the global span timing table accumulated since the server started (or
-- since the last DELETE). Set ARKHAM_METRICS=1 in the env to enable.
getApiV1AdminMetricsR :: Handler Value
getApiV1AdminMetricsR = do
  mref <- liftIO Metrics.isMetricsEnabled
  case mref of
    Nothing ->
      pure
        $ Aeson.object
          [ "enabled" Aeson..= False
          , "hint" Aeson..= ("Set ARKHAM_METRICS=1 in the environment and restart the server" :: Text)
          ]
    Just ref -> do
      txt <- liftIO $ Metrics.formatMetrics ref 200
      pure
        $ Aeson.object
          [ "enabled" Aeson..= True
          , "table" Aeson..= txt
          ]

-- DELETE /api/v1/admin/metrics
--
-- Resets the accumulator without disabling collection. Useful for
-- "start measuring fresh from this user action".
deleteApiV1AdminMetricsR :: Handler Value
deleteApiV1AdminMetricsR = do
  liftIO Metrics.resetMetrics
  pure
    $ Aeson.object
      [ "ok" Aeson..= True
      ]
