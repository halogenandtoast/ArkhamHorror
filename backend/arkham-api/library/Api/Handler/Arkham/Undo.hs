{-# LANGUAGE OverloadedRecordDot #-}

module Api.Handler.Arkham.Undo (
  putApiV1ArkhamGameUndoR,
  putApiV1ArkhamGameUndoScenarioR,
) where

import Import hiding (delete, on, update, (<.), (=.), (==.), (>=.))

import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Card.CardCode
import Arkham.Game
import Arkham.Game.Diff
import Arkham.Id
import Control.Lens (view)
import Data.Aeson.Patch
import Data.Text qualified as T
import Data.These
import Data.Time.Clock
import Database.Esqueleto.Experimental
import Entity.Arkham.LogEntry
import Entity.Arkham.Step
import Json
import Network.HTTP.Types.Status qualified as Status
import Safe (fromJustNote)

stepBackUntil :: UserId -> ArkhamGameId -> ArkhamGame -> (ArkhamGame -> Bool) -> Handler ArkhamGame
stepBackUntil userId gameId game f = do
  if not (f game)
    then do
      game' <- stepBack userId gameId game
      stepBackUntil userId gameId game' f
    else pure game

jsonError :: Text -> Handler a
jsonError msg = sendStatusJSON Status.status400 (object ["error" .= msg])

jsonErrorContents :: ToJSON v => v -> Text -> Handler a
jsonErrorContents v msg = sendStatusJSON Status.status400 (object ["error" .= msg, "contents" .= v])

stepBack :: UserId -> ArkhamGameId -> ArkhamGame -> Handler ArkhamGame
stepBack userId gameId current@ArkhamGame {..} = do
  Entity pid arkhamPlayer <- runDB $ getBy404 (UniquePlayer userId gameId)
  mstep <- runDB $ getBy (UniqueStep gameId arkhamGameStep)
  case mstep of
    Nothing -> jsonError "Missing step"
    Just (Entity stepId step) -> do
      -- never delete the initial step as it can not be redone
      -- NOTE: actually we never want to step back if the patchOperations are empty, the first condition is therefor redundant
      when (arkhamStepStep step <= 0) $ jsonErrorContents step "Can't undo the first step"
      when (null $ patchOperations $ choicePatchDown $ arkhamStepChoice step) do
        -- we don't need to apply any real updates so let's just remove the step
        arkhamGame <- runDB $ do
          void $ select do
            game <- from $ table @ArkhamGame
            where_ $ game.id ==. val gameId
            locking ForUpdate
            pure ()
          -- ensure previous step exists
          maybe (error $ "can not go back, at step: " <> tshow arkhamGameStep) (\_ -> pure ())
            =<< getBy (UniqueStep gameId (arkhamGameStep - 1))

          update \g -> do
            set g [ArkhamGameStep =. val (arkhamGameStep - 1)]
            where_ $ g.id ==. val gameId

          delete $ do
            entries <- from $ table @ArkhamLogEntry
            where_ $ entries.arkhamGameId ==. val gameId
            where_ $ entries.step >=. val (arkhamGameStep - 1)
          deleteKey stepId
          pure $ current {arkhamGameStep = arkhamGameStep - 1}

        sendStatusJSON Status.status200 arkhamGame
      case patch arkhamGameCurrentData (choicePatchDown $ arkhamStepChoice step) of
        Error e -> error $ T.pack e
        Success ge -> do
          -- TODO: We need to add back the gameActionDiff
          now <- liftIO getCurrentTime
          arkhamGame <- runDB $ do
            void $ select do
              game <- from $ table @ArkhamGame
              where_ $ game.id ==. val gameId
              locking ForUpdate
              pure ()
            -- ensure previous step exists
            maybe (error $ "can not go back, at step: " <> tshow arkhamGameStep) (\_ -> pure ())
              =<< getBy (UniqueStep gameId (arkhamGameStep - 1))

            let arkhamGame =
                  ArkhamGame
                    arkhamGameName
                    ge
                    (arkhamGameStep - 1)
                    arkhamGameMultiplayerVariant
                    arkhamGameCreatedAt
                    now

            replace gameId arkhamGame
            delete $ do
              entries <- from $ table @ArkhamLogEntry
              where_ $ entries.arkhamGameId ==. val gameId
              where_ $ entries.step >=. val (arkhamGameStep - 1)
            deleteKey stepId

            case arkhamGameMultiplayerVariant of
              Solo ->
                replace pid
                  $ arkhamPlayer
                    { arkhamPlayerInvestigatorId = coerce (view activeInvestigatorIdL ge)
                    }
              WithFriends -> pure ()
            pure arkhamGame
          pure arkhamGame

putApiV1ArkhamGameUndoR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  game <- runDB $ get404 gameId
  ArkhamGame {..} <- stepBack userId gameId game
  room <- getRoom gameId
  let writeChannel = socketChannel room

  gameLog <- fmap (fmap unValue)
    . runDB
    $ select
    $ do
      entries <- from $ table @ArkhamLogEntry
      where_ $ entries.arkhamGameId ==. val gameId
      where_ $ entries.step <. val (arkhamGameStep - 1)
      orderBy [desc entries.createdAt]
      pure $ entries.body

  atomically
    $ writeTChan writeChannel
    $ encode
    $ GameUpdate
    $ PublicGame gameId arkhamGameName gameLog arkhamGameCurrentData

putApiV1ArkhamGameUndoScenarioR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoScenarioR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  game <- runDB $ get404 gameId
  ArkhamGame {..} <- stepBackUntil userId gameId game \ArkhamGame {..} -> do
    case gameMode arkhamGameCurrentData of
      These _ _ -> False
      _ -> True

  room <- getRoom gameId
  let writeChannel = socketChannel room

  gameLog <- fmap (fmap unValue)
    . runDB
    $ select
    $ do
      entries <- from $ table @ArkhamLogEntry
      where_ $ entries.arkhamGameId ==. val gameId
      where_ $ entries.step <. val (arkhamGameStep - 1)
      orderBy [desc entries.createdAt]
      pure $ entries.body

  atomically
    $ writeTChan writeChannel
    $ encode
    $ GameUpdate
    $ PublicGame gameId arkhamGameName gameLog arkhamGameCurrentData
