{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GHCI (
  module GHCI,
  module X,
) where

import Import.NoFoundation as X

import Api.Arkham.Helpers
import Application
import Arkham.Classes.Entity
import Arkham.Classes.HasQueue
import Arkham.Game
import Arkham.Id
import Arkham.Message
import Arkham.Scenario.Types
import Arkham.Tarot
import Control.Lens (ix, (.~))
import Control.Monad.Logger
import Control.Monad.Random (mkStdGen)
import Data.Maybe (fromJust)
import Data.String.Conversions (cs)
import Data.These
import Data.Time
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.Persist.Postgresql

instance IsString UUID where
  fromString = fromJust . UUID.fromString

instance IsString (Key ArkhamGame) where
  fromString = ArkhamGameKey . fromString

instance IsString EventId where
  fromString = EventId . fromString

instance IsString LocationId where
  fromString = LocationId . fromString

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

runGameMessage :: UUID -> Message -> IO ()
runGameMessage gameUUID msg = do
  let gameId = ArkhamGameKey gameUUID
  ArkhamGame {..} <- dbGhci $ get404 gameId
  let Game {gameSeed} = arkhamGameCurrentData
  gameRef <- newIORef arkhamGameCurrentData
  queueRef <- newQueue [msg]
  genRef <- newIORef (mkStdGen gameSeed)
  runGameApp
    (GameApp gameRef queueRef genRef $ pure . const ())
    (runMessages Nothing)
  ge <- readIORef gameRef
  now <- liftIO getCurrentTime
  void $ dbGhci $ do
    replace gameId
      $ ArkhamGame
        arkhamGameName
        ge
        arkhamGameStep
        arkhamGameMultiplayerVariant
        arkhamGameCreatedAt
        now

gameDB :: Game -> ReaderT Game IO a -> IO a
gameDB = flip runReaderT

setTarot :: UUID -> [TarotCard] -> IO ()
setTarot gameUUID cards = do
  let gameId = ArkhamGameKey gameUUID
  ArkhamGame {..} <- dbGhci $ get404 gameId
  let Game {gameMode} = arkhamGameCurrentData
  let
    gameMode' = case gameMode of
      That s -> That $ overAttrs (tarotCardsL . ix GlobalTarot .~ cards) s
      These c s -> These c $ overAttrs (tarotCardsL . ix GlobalTarot .~ cards) s
      c -> c
  now <- liftIO getCurrentTime
  void $ dbGhci $ do
    replace gameId
      $ ArkhamGame
        arkhamGameName
        (arkhamGameCurrentData {gameMode = gameMode'})
        arkhamGameStep
        arkhamGameMultiplayerVariant
        arkhamGameCreatedAt
        now
