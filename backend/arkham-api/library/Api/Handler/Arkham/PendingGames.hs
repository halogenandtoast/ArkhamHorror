{-# LANGUAGE OverloadedRecordDot #-}

module Api.Handler.Arkham.PendingGames (
  getApiV1ArkhamPendingGameR,
  putApiV1ArkhamPendingGameR,
) where

import Import hiding (on, (==.))

import Api.Arkham.Helpers
import Arkham.Classes.HasQueue
import Arkham.Game
import Arkham.Game.State
import Arkham.Id
import Arkham.Queue
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Data.Aeson
import Data.Time.Clock
import Database.Persist ((==.))
import Entity.Arkham.Step
import OpenTelemetry.Trace.Monad (MonadTracer (..))

getApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (PublicGame ArkhamGameId)
getApiV1ArkhamPendingGameR gameId = do
  _ <- getRequestUserId
  ge <- runDB $ get404 gameId
  pure $ toPublicGame (Entity gameId ge) mempty

putApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (PublicGame ArkhamGameId)
putApiV1ArkhamPendingGameR gameId = do
  userId <- getRequestUserId
  tracer <- getTracer
  now <- liftIO getCurrentTime
  game@ArkhamGame {..} <- runDB $ atomicallyWithGame gameId \original@ArkhamGame {..} -> do
    case gameGameState arkhamGameCurrentData of
      IsPending _ -> do
        alreadyExists <- exists [ArkhamPlayerArkhamGameId ==. gameId, ArkhamPlayerUserId ==. userId]

        if alreadyExists
          then pure original
          else do
            mLastStep <- getBy (UniqueStep gameId arkhamGameStep)
            let currentQueue = maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

            gameRef <- liftIO $ newIORef arkhamGameCurrentData
            queueRef <- liftIO $ newQueue currentQueue
            genRef <- liftIO $ newIORef (mkStdGen (gameSeed arkhamGameCurrentData))

            pid <- insert $ ArkhamPlayer userId gameId "00000"

            runGameApp (GameApp gameRef queueRef genRef (pure . const ()) tracer) $ do
              addPlayer (PlayerId $ coerce pid)
              runMessages (gameIdToText gameId) Nothing

            updatedGame <- liftIO $ readIORef gameRef
            updatedQueue <- liftIO $ readIORef (queueToRef queueRef)

            let
              game' =
                ArkhamGame
                  arkhamGameName
                  updatedGame
                  (arkhamGameStep + 1)
                  arkhamGameMultiplayerVariant
                  arkhamGameCreatedAt
                  now

            replace gameId game'
            insert_
              $ ArkhamStep
                gameId
                (Choice mempty updatedQueue)
                (arkhamGameStep + 1)
                (ActionDiff $ view actionDiffL updatedGame)

            pure game' 
      _ -> pure original

  writeChannel <- (.channel) <$> getRoom gameId
  atomically
    $ writeTChan writeChannel
    $ encode
    $ GameUpdate
    $ PublicGame gameId arkhamGameName [] arkhamGameCurrentData
  pure $ toPublicGame (Entity gameId game) mempty
