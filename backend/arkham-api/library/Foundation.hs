{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Foundation where

import Import.NoFoundation

import Auth.JWT qualified as JWT
import Control.Monad.Logger (LogSource)
import Data.Aeson (Result (Success), fromJSON)
import Data.ByteString.Lazy qualified as BSL
import Database.Persist.Sql (
  ConnectionPool,
  SqlBackend,
  SqlPersistT,
  runSqlPool,
 )
import Database.Redis (Connection, PubSubController, RedisChannel)
import GHC.Records
import Network.HTTP.Client.Conduit (HasHttpManager (..), Manager)
import Yesod.Core.Types (Logger)
import Yesod.Core.Unsafe qualified as Unsafe

import Arkham.Card.CardCode

import Orphans ()

data Room = Room
  { socketChannel :: TChan BSL.ByteString
  , socketClients :: Int
  , messageBrokerChannel :: RedisChannel
  }

instance HasField "channel" Room (TChan BSL.ByteString) where
  getField = socketChannel

instance HasField "clients" Room Int where
  getField = socketClients

instance HasField "broker" Room RedisChannel where
  getField = messageBrokerChannel

{- | The foundation datatype for your application. This can be a good place to
keep settings and values requiring initialization before your application
starts running, such as database connections. Every handler will have
access to the data present here.
-}
data App = App
  { appSettings :: AppSettings
  , appConnPool :: ConnectionPool
  , appRedis :: Connection
  , appPubSub :: PubSubController
  -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger :: Logger
  , appGameRooms :: !(IORef (Map ArkhamGameId Room))
  }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type). MonadIO m => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot :: Approot App
  approot = ApprootRequest $ \app req -> case appRoot $ appSettings app of
    Nothing -> getApprootText guessApproot app req
    Just root -> root

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ =
    Just
      <$> defaultClientSessionBackend
        120 -- timeout in minutes
        "config/client_session_key.aes"

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware = defaultYesodMiddleware

  defaultLayout :: Widget -> Handler Html
  defaultLayout _ = pure ""

  isAuthorized
    :: Route App
    -- \^ The route the user is visiting.
    -> Bool
    -- \^ Whether or not this is a "write" request.
    -> Handler AuthResult
  -- Routes not requiring authentication.
  isAuthorized HealthR _ = pure Authorized
  isAuthorized (ApiP _) _ = pure Authorized

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    pure
      $ appShouldLogAll (appSettings app)
      || level
      == LevelWarn
      || level
      == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = pure . appLogger

  maximumContentLength :: App -> Maybe (Route App) -> Maybe Word64
  maximumContentLength _ _ = Just $ 200 * 1024 * 1024

class Monad m => CanRunDB m where
  runDB :: SqlPersistT m a -> m a

instance CanRunDB (HandlerFor App) where
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

-- How to run database actions.
-- instance YesodPersist App where
--   type YesodPersistBackend App = SqlBackend
--   runDB :: SqlPersistT Handler a -> Handler a
--   runDB action = do
--     master <- getYesod
--     runSqlPool action $ appConnPool master
--
-- instance YesodPersistRunner App where
--   getDBRunner :: Handler (DBRunner App, Handler ())
--   getDBRunner = defaultGetDBRunner appConnPool

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

userIdToToken :: UserId -> HandlerFor App Text
userIdToToken userId = do
  jwtSecret <- getJwtSecret
  pure $ JWT.jsonToToken jwtSecret $ toJSON userId

tokenToUserId :: Text -> Handler (Maybe UserId)
tokenToUserId token = do
  jwtSecret <- getJwtSecret
  let mUserId = fromJSON <$> JWT.tokenToJson jwtSecret token
  case mUserId of
    Just (Success userId) -> pure $ Just userId
    _ -> pure Nothing

getJwtSecret :: HandlerFor App Text
getJwtSecret = getsYesod $ appJwtSecret . appSettings

getRequestUserId :: Handler (Maybe UserId)
getRequestUserId = do
  mToken <- JWT.lookupToken
  liftHandler $ maybe (pure Nothing) tokenToUserId mToken

getEntity404
  :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend) => Key record -> DB (Entity record)
getEntity404 key = Entity key <$> get404 key
