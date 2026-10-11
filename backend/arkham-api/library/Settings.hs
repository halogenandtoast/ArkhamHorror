{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStrictData #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import Control.Exception qualified as Exception
import Data.Aeson
  (FromJSON(..), Result(..), Value, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.ByteString.Char8 qualified as BS8
import Data.FileEmbed (embedFile)
import Data.String.Conversions.Monomorphic (toStrictByteString)
import Data.Text qualified as T
import Data.Yaml (decodeEither')
import Data.Yaml.Config (applyEnvValue)
import Database.Persist.Postgresql (PostgresConf(..))
import Network.Wai.Handler.Warp (HostPreference)
import Network.Mail.Mailtrap (Token)
import Relude
import URI.ByteString
  ( Authority(..)
  , Host(..)
  , Port(..)
  , Scheme(..)
  , URIRef(..)
  , UserInfo(..)
  , parseURI
  , strictURIParserOptions
  )

abortLeft :: (MonadFail m, Show e) => Either e b -> m b
abortLeft = either (fail . ("DATABASE_URL failed to parse: " <>) . show) pure

abortNothing :: MonadFail m => String -> Maybe a -> m a
abortNothing s = maybe (fail $ "DATABASE_URL is missing " <> s) pure

fromDatabaseUrl :: (MonadFail m) => Int -> Text -> m PostgresConf
fromDatabaseUrl size url = do
  uri <- abortLeft $ parseURI strictURIParserOptions $ toStrictByteString url
  auth <- abortNothing "authority" $ uriAuthority uri
  port <- abortNothing "port" $ authorityPort auth
  dbName <- abortNothing "path" $ snd <$> BS8.uncons (uriPath uri)
  unless (schemeBS (uriScheme uri) == "postgres") $ fail "DATABASE_URL has unknown scheme"
  pure PostgresConf
    { pgConnStr =
        maybe "" (\info -> "user=" <> uiUsername info <> " password=" <> uiPassword info) (authorityUserInfo auth)
        <> " host=" <> hostBS (authorityHost auth)
        <> " port=" <> BS8.pack (show $ portNumber port)
        <> " dbname=" <> dbName
    , pgPoolSize = size
    , pgPoolStripes = 1
    , pgPoolIdleTimeout = 60
    }

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining
    , appJwtSecret :: Text
    , appRedisConnectionInfo :: Maybe Text
    -- ^ Redis Connection Info
    , appMailtrapApiToken :: Token
    , appBugsnagApiKey :: Text
    , appAssetHost :: Maybe Text
    , appCustomCardArtDir :: Maybe FilePath
    -- ^ Where to put custom card art. In development it goes in the frontend's
    -- public directory so it is served locally and never reaches the shared
    -- assets bucket; unset (the default outside development) means S3.
    , appArkhamBuildApiBase :: Text
    -- ^ arkham.build's API origin, @https://api.arkham.build@.
    , appArkhamBuildClientId :: Maybe Text
    , appArkhamBuildClientSecret :: Maybe Text
    -- ^ Unset means the integration is off: no authorization can be started and
    -- the settings page says so rather than offering a button that cannot work.
    , appArkhamBuildRedirectUri :: Text
    -- ^ Compared by arkham.build as an exact string against what an
    -- administrator registered, and sent again at the token exchange, so it is
    -- configured rather than derived from the request.
    , appMaintenanceToken :: Maybe Text
    -- ^ Shared secret for the maintenance switch
    -- ("Api.Handler.Arkham.Maintenance"). Unset means the endpoint does not
    -- exist at all, so an unconfigured deployment cannot have its maintenance
    -- mode flipped by anyone who guesses the route.
    --
    -- A token rather than an admin login because 'Foundation.getAdminUser'
    -- reads the users table: authenticating the one endpoint whose job is to
    -- lift a lock on the database would make it the first thing to queue
    -- behind that lock.
    , appArchiveBucket :: Maybe Text
    -- ^ S3 bucket that offloaded undo histories are written to
    -- ("Api.Arkham.Archive"). Unset means the whole mechanism is off: no sweep
    -- runs, nothing is uploaded, and no game can be archived or restored. A
    -- deployment that has not been given somewhere to put the data must not
    -- start deleting steps on the strength of a default bucket name.
    , appArchiveSweepEnabled :: Bool
    {- ^ Whether the daily sweep runs at all. __Off by default__, which is not
    the same as the feature being off: with a bucket configured but this unset,
    archiving and restoring work and are only ever started by hand, from the
    admin panel. That is the order to do this in -- archive one game you picked,
    restore it, and only then let a job loose on twenty thousand.
    -}
    , appArchiveAfterMonths :: Int
    -- ^ How long a game must have gone untouched before its undo history is a
    -- candidate for the sweep. A hand-picked game is not subject to it.
    , appArchiveBatchSize :: Int
    -- ^ Games per batch. The sweep works in batches with a pause between them
    -- rather than in one pass, so the deletes land as a trickle against the
    -- busiest table in the application instead of as one burst.
    , appArchiveBatchPauseSeconds :: Int
    -- ^ Seconds between batches, and so the whole throttle: a run drains the
    -- entire backlog, and this is what decides how fast it does it. 10 games
    -- every 20s is ~1,800 an hour.
    , appWebsocketCompression :: Bool
    -- ^ permessage-deflate on the game/event websockets. Defaults on; it takes
    -- a 206 KB 'PublicGame' update to ~33 KB. It is also the one thing on that
    -- path that costs CPU and allocation per message, and the load only shows
    -- up under real concurrency, so keep it switchable in production without a
    -- rebuild: @ARKHAM_WS_COMPRESSION=false@. See
    -- 'Api.Handler.Arkham.Games.Shared.websocketConnectionOptions'.
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#ifdef DEVELOPMENT
                True
#else
                False
#endif
        poolSize                  <- o .: "database-pool-size"
        appDatabaseConf           <- fromDatabaseUrl poolSize =<< (o .: "database-url")
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        dev                       <- o .:? "development"      .!= defaultDev

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
        appShouldLogAll           <- o .:? "should-log-all"   .!= dev
        appReloadTemplates        <- o .:? "reload-templates" .!= dev
        appSkipCombining          <- o .:? "skip-combining"   .!= dev
        appJwtSecret <- o .: "jwt-secret"
        appRedisConnectionInfo <- o .:? "redis-conn"
        appMailtrapApiToken <- o .: "mailtrap-api-token"
        appBugsnagApiKey <- o .: "bugsnag-api-token"
        appAssetHost <- o .:? "asset-host"
        mArtDir <- o .:? "custom-card-art-dir"
        let appCustomCardArtDir = case mArtDir of
                Just "" -> Nothing
                Just dir -> Just dir
                -- Relative to the api package, which is where it is run from.
                Nothing -> if dev then Just "../../frontend/public/img/custom" else Nothing
        appArkhamBuildApiBase <- o .:? "arkham-build-api-base" .!= "https://api.arkham.build"
        appArkhamBuildClientId <- nonEmpty' <$> o .:? "arkham-build-client-id"
        appArkhamBuildClientSecret <- nonEmpty' <$> o .:? "arkham-build-client-secret"
        mRedirectUri <- nonEmpty' <$> o .:? "arkham-build-redirect-uri"
        let appArkhamBuildRedirectUri = fromMaybe
                (if dev
                    then "http://localhost:8080/api/auth/arkham-build/callback"
                    else "https://arkhamhorror.app/api/auth/arkham-build/callback")
                mRedirectUri
        appMaintenanceToken <- nonEmpty' <$> o .:? "maintenance-token"
        appArchiveBucket <- nonEmpty' <$> o .:? "archive-bucket"
        appArchiveSweepEnabled <- o .:? "archive-sweep" .!= False
        appArchiveAfterMonths <- o .:? "archive-after-months" .!= 6
        appArchiveBatchSize <- o .:? "archive-batch-size" .!= 10
        appArchiveBatchPauseSeconds <- o .:? "archive-batch-pause-seconds" .!= 20
        appWebsocketCompression <- o .:? "websocket-compression" .!= True
        pure AppSettings {..}

nonEmpty' :: Maybe Text -> Maybe Text
nonEmpty' = (>>= \t -> if T.null (T.strip t) then Nothing else Just (T.strip t))

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile "config/settings.yml")

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error . T.pack $ e
        Success settings -> settings
