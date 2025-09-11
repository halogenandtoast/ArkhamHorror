{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Foundation where

import Import.NoFoundation

import Control.Exception.Annotated qualified as UE
import Control.Exception.Annotated.UnliftIO qualified as AnnotatedIO
import Control.Monad.Catch (
  MonadCatch (..),
  MonadMask (..),
  MonadThrow (..),
  generalBracket,
 )
import Control.Monad.Catch qualified as Catch
import UnliftIO.Exception qualified as UnliftIO

import Arkham.Card.CardCode
import Auth.JWT qualified as JWT
import Control.Monad.Logger (LogSource)
import Data.Aeson (Result (Success), fromJSON)
import Data.Bugsnag.Settings qualified as Bugsnag
import Data.ByteString.Lazy qualified as BSL
import Data.Traversable (for)
import Database.Persist.Sql (
  ConnectionPool,
  SqlBackend,
  SqlPersistT,
  runSqlPool,
 )
import Database.Redis (
  Connection,
  MessageCallback,
  PubSubController,
  RedisChannel,
  addChannels,
  removeChannels,
 )
import GHC.Records
import Network.Bugsnag.Exception (AsException (..))
import Network.Bugsnag.Yesod (bugsnagYesodMiddleware)
import Network.HTTP.Client.Conduit (HasHttpManager (..), Manager)
import Orphans ()
import Yesod.Core.Types (Logger)
import Yesod.Core.Unsafe qualified as Unsafe
import "bugsnag" Network.Bugsnag qualified as Bugsnag
import "bugsnag-hs" Network.Bugsnag qualified as Bugsnag (Exception)

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

data MessageBroker = WebSocketBroker | RedisBroker Connection PubSubController

addChannel :: (MonadIO m, HasApp m) => RedisChannel -> MessageCallback -> m ()
addChannel chn handleIt = do
  msgBroker <- getsApp appMessageBroker
  case msgBroker of
    WebSocketBroker -> pure ()
    RedisBroker _ ctrl -> void $ addChannels ctrl [(chn, handleIt)] []

removeChannel :: (MonadIO m, HasApp m) => RedisChannel -> m ()
removeChannel chn = do
  msgBroker <- getsApp appMessageBroker
  case msgBroker of
    WebSocketBroker -> pure ()
    RedisBroker _ ctrl -> void $ removeChannels ctrl [chn] []

{- | The foundation datatype for your application. This can be a good place to
keep settings and values requiring initialization before your application
starts running, such as database connections. Every handler will have
access to the data present here.
-}
data App = App
  { appSettings :: AppSettings
  , appConnPool :: ConnectionPool
  , appMessageBroker :: MessageBroker
  -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger :: Logger
  , appGameRooms :: !(IORef (Map ArkhamGameId Room))
  , appBugsnag :: Bugsnag.Settings
  }

class Monad m => HasApp m where
  getApp :: m App

getsApp :: HasApp m => (App -> a) -> m a
getsApp f = f <$> getApp

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
  yesodMiddleware = bugsnagYesodMiddleware appBugsnag . defaultYesodMiddleware

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
  isAuthorized ErrorR _ = pure Authorized
  isAuthorized (ApiP api) _ = case api of
    ApiV1P v1 -> case v1 of
      AdminP _ -> do
        _ <- getAdminUser
        pure Authorized
      ApiV1ArkhamP arkham -> case arkham of
        ApiV1ArkhamGamesP games -> case games of
          ApiV1ArkhamGamesImportR -> pure Authorized
          ApiV1ArkhamGamesFixR -> do
            _ <- getAdminUser
            pure Authorized
          ApiV1ArkhamGamesReloadR -> do
            _ <- getAdminUser
            pure Authorized
          ApiV1ArkhamGameP _ game -> case game of
            ApiV1ArkhamGameFullExportR -> do
              _ <- getAdminUser
              pure Authorized
            ApiV1ArkhamGameReloadR -> do
              _ <- getAdminUser
              pure Authorized
            _ -> pure Authorized
          _ -> pure Authorized
        _ -> pure Authorized
      _ -> pure Authorized

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

bugsnag :: (HasBugsnag m, MonadIO m) => Bugsnag.Exception -> m ()
bugsnag e = do
  settings <- bugsnagSettings
  liftIO $ Bugsnag.notifyBugsnag settings (AsException e)

class HasBugsnag m where
  bugsnagSettings :: m Bugsnag.Settings

instance HasApp m => HasBugsnag m where
  bugsnagSettings = getsApp appBugsnag

instance HasApp (HandlerFor App) where
  getApp = getYesod

instance CanRunDB (HandlerFor App) where
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

newtype ExceptionViaIO m a = ExceptionViaIO (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

{- | A variant of 'throwWithCallStackIO' that uses 'MonadIO' instead of
'MonadThrow'.
-}
throwWithCallStackIO
  :: (MonadIO m, Exception e)
  => e
  -> m a
throwWithCallStackIO = liftIO . withFrozenCallStack UE.throwWithCallStack

instance MonadIO m => MonadThrow (ExceptionViaIO m) where
  throwM = throwWithCallStackIO

instance MonadUnliftIO m => MonadCatch (ExceptionViaIO m) where
  catch = AnnotatedIO.catch

instance MonadUnliftIO m => MonadMask (ExceptionViaIO m) where
  generalBracket = generalBracketIO
  mask = UnliftIO.mask
  uninterruptibleMask = UnliftIO.uninterruptibleMask

deriving via ExceptionViaIO (HandlerFor app) instance MonadMask (HandlerFor app)
deriving via ExceptionViaIO (HandlerFor app) instance MonadCatch (HandlerFor app)

generalBracketIO
  :: MonadUnliftIO m
  => m a
  -> (a -> Catch.ExitCase b -> m c)
  -> (a -> m b)
  -> m (b, c)
generalBracketIO acquire release action = do
  UnliftIO.mask \restore -> do
    x <- acquire
    res1 <- UnliftIO.try $ restore $ action x
    case res1 of
      Left e1 -> do
        -- explicitly ignore exceptions from after
        _ :: Either SomeException c <-
          UnliftIO.try $ UnliftIO.uninterruptibleMask_ $ release x (Catch.ExitCaseException e1)
        throwWithCallStackIO e1
      Right y -> do
        c <- UnliftIO.uninterruptibleMask_ $ release x (Catch.ExitCaseSuccess y)
        pure (y, c)

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
  liftIO $ JWT.jsonToToken jwtSecret $ toJSON userId

tokenToUserId :: Text -> Handler (Maybe UserId)
tokenToUserId token = do
  jwtSecret <- getJwtSecret
  mUserId <- liftIO $ fmap fromJSON <$> JWT.tokenToJson jwtSecret token
  case mUserId of
    Just (Success userId) -> pure $ Just userId
    _ -> pure Nothing

getJwtSecret :: HandlerFor App Text
getJwtSecret = getsYesod $ appJwtSecret . appSettings

getRequestUserId :: Handler UserId
getRequestUserId = do
  mToken <- JWT.lookupToken
  maybe notAuthenticated pure . join =<< for mToken tokenToUserId

getAdminUser :: Handler (Entity User)
getAdminUser = do
  userId <- getRequestUserId
  user <- runDB $ get404 userId
  unless user.admin $ permissionDenied "You must be an admin to access this endpoint"
  pure $ Entity userId user

getRequestUser :: Handler (Entity User)
getRequestUser = do
  userId <- getRequestUserId
  runDB $ getEntity404 userId

getEntity404
  :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend) => Key record -> DB (Entity record)
getEntity404 key = Entity key <$> get404 key
