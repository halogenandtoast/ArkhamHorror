module GHCI
  ( module GHCI
  , module X
  ) where

import Import.NoFoundation as X

import Api.Arkham.Helpers
import Application
import Arkham.Game
import Arkham.Message
import Control.Monad.Logger
import Control.Monad.Random ( mkStdGen )
import Data.String.Conversions ( cs )
import Data.Time
import Data.UUID qualified as UUID
import Database.Persist.Postgresql

dbGhci :: SqlPersistT IO a -> IO a
dbGhci action = do
  appSettings <- getAppSettings
  let conf = appDatabaseConf appSettings
  dbURL <- maybe (pgConnStr conf) cs <$> lookupEnv "DATABASE_URL"
  pool <- runStdoutLoggingT $ createPostgresqlPool dbURL (pgPoolSize conf)
  runSqlPool action pool

runGameMessage :: String -> Message -> IO ()
runGameMessage gameUUID msg = do
  let
    gameId =
      maybe (error "invalid uuid") ArkhamGameKey $ UUID.fromString gameUUID
  ArkhamGame {..} <- dbGhci $ get404 gameId
  let Game { gameSeed } = arkhamGameCurrentData
  gameRef <- newIORef arkhamGameCurrentData
  queueRef <- newIORef [msg]
  genRef <- newIORef (mkStdGen gameSeed)
  runGameApp
    (GameApp gameRef queueRef genRef $ pure . const ())
    (runMessages Nothing)
  ge <- readIORef gameRef
  now <- liftIO getCurrentTime
  void $ dbGhci $ do
    replace gameId $ ArkhamGame
      arkhamGameName
      ge
      arkhamGameChoices
      arkhamGameLog
      arkhamGameMultiplayerVariant
      arkhamGameCreatedAt
      now
