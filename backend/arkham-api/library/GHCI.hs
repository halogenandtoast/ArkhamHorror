{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GHCI
  ( module GHCI
  , module X
  ) where

import Import.NoFoundation as X hiding (selectList)

import Api.Arkham.Helpers
import Application
import Arkham.Classes.HasQueue
import Arkham.Game
import Arkham.Message
import Control.Monad.Logger
import Control.Monad.Random ( mkStdGen )
import Data.String.Conversions ( cs )
import Data.Maybe (fromJust)
import Data.Time
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.Persist.Postgresql hiding (selectList)

instance IsString UUID where
  fromString = fromJust . UUID.fromString

instance IsString (Key ArkhamGame) where
  fromString = ArkhamGameKey . fromString

lookupGame :: Key ArkhamGame -> IO Game
lookupGame gameId = do
  ArkhamGame {..} <- dbGhci $ get404 gameId
  pure arkhamGameCurrentData

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
  queueRef <- newQueue [msg]
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
      arkhamGameStep
      arkhamGameMultiplayerVariant
      arkhamGameCreatedAt
      now

gameDB :: Game -> ReaderT Game IO a -> IO a
gameDB g = flip runReaderT g
