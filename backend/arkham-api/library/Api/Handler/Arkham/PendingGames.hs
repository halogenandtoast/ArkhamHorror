{-# LANGUAGE OverloadedRecordDot #-}

module Api.Handler.Arkham.PendingGames (
  getApiV1ArkhamPendingGameR,
  putApiV1ArkhamPendingGameR,
) where

import Import hiding (on, (==.))

import Api.Arkham.Helpers
import Arkham.Classes.HasQueue
import Arkham.Game
import Arkham.Id
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Data.Aeson
import Data.Time.Clock
import Entity.Arkham.Step
import Safe (fromJustNote)

getApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (PublicGame ArkhamGameId)
getApiV1ArkhamPendingGameR gameId = do
  _ <- fromJustNote "Not authenticated" <$> getRequestUserId
  ge <- runDB $ get404 gameId
  pure $ toPublicGame (Entity gameId ge) mempty

putApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (PublicGame ArkhamGameId)
putApiV1ArkhamPendingGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  ArkhamGame {..} <- runDB $ get404 gameId

  mLastStep <- runDB $ getBy (UniqueStep gameId arkhamGameStep)
  let
    currentQueue =
      maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

  gameRef <- newIORef arkhamGameCurrentData
  queueRef <- newQueue currentQueue
  genRef <- newIORef (mkStdGen (gameSeed arkhamGameCurrentData))

  pid <- runDB $ insert $ ArkhamPlayer userId gameId "00000"

  runGameApp (GameApp gameRef queueRef genRef (pure . const ())) $ do
    addPlayer (PlayerId $ coerce pid)
    runMessages Nothing

  updatedGame <- readIORef gameRef
  updatedQueue <- readIORef (queueToRef queueRef)

  writeChannel <- (.channel) <$> getRoom gameId
  atomically
    $ writeTChan writeChannel
    $ encode
    $ GameUpdate
    $ PublicGame gameId arkhamGameName [] updatedGame

  now <- liftIO getCurrentTime

  let
    game' =
      ArkhamGame
        arkhamGameName
        updatedGame
        (arkhamGameStep + 1)
        arkhamGameMultiplayerVariant
        arkhamGameCreatedAt
        now

  runDB $ do
    replace gameId game'
    insert_
      $ ArkhamStep
        gameId
        (Choice mempty updatedQueue)
        (arkhamGameStep + 1)
        (ActionDiff $ view actionDiffL updatedGame)

  pure $ toPublicGame (Entity gameId game') mempty
