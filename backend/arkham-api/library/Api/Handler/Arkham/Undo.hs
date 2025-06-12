module Api.Handler.Arkham.Undo (putApiV1ArkhamGameUndoR, putApiV1ArkhamGameUndoScenarioR) where

import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Card.CardCode
import Arkham.Game
import Arkham.Game.Diff
import Arkham.Id
import Control.Lens (view)
import Control.Monad.Random (getRandom)
import Data.Aeson.Patch
import Data.Text qualified as T
import Data.Time.Clock
import Database.Esqueleto.Experimental
import Entity.Arkham.LogEntry
import Entity.Arkham.Step
import Import hiding (delete, on, update, (<.), (=.), (==.), (>=.))
import Json
import Network.HTTP.Types.Status qualified as Status
import Safe (fromJustNote)

jsonError :: Text -> Handler a
jsonError msg = sendStatusJSON Status.status400 (object ["error" .= msg])

jsonErrorContents :: ToJSON v => v -> Text -> Handler a
jsonErrorContents v msg = sendStatusJSON Status.status400 (object ["error" .= msg, "contents" .= v])

stepBack :: UserId -> ArkhamGameId -> ArkhamGame -> Handler ArkhamGame
stepBack userId gameId current@ArkhamGame {..} = do
  Entity pid arkhamPlayer <- runDB $ getBy404 (UniquePlayer userId gameId)
  runDB (getBy (UniqueStep gameId arkhamGameStep)) >>= \case
    Nothing -> jsonError "Missing step"
    Just (Entity stepId step) -> do
      -- never delete the initial step as it can not be redone
      -- NOTE: actually we never want to step back if the patchOperations are empty, the first condition is therefor redundant
      when (arkhamStepStep step <= 0) $ jsonErrorContents step "Can't undo the first step"
      when (null $ patchOperations $ choicePatchDown $ arkhamStepChoice step) do
        -- we don't need to apply any real updates so let's just remove the step
        arkhamGame <- runDB do
          void $ select do
            game <- from $ table @ArkhamGame
            where_ $ game.id ==. val gameId
            locking forUpdate
          -- ensure previous step exists
          maybe (error $ "can not go back, at step: " <> tshow arkhamGameStep) (\_ -> pure ())
            =<< getBy (UniqueStep gameId (arkhamGameStep - 1))

          update \g -> do
            set g [ArkhamGameStep =. val (arkhamGameStep - 1)]
            where_ $ g.id ==. val gameId

          delete do
            entries <- from $ table @ArkhamLogEntry
            where_ $ entries.arkhamGameId ==. val gameId
            where_ $ entries.step >=. val (arkhamGameStep - 1)
          deleteKey stepId
          pure $ current {arkhamGameStep = arkhamGameStep - 1}

        sendStatusJSON Status.status200 arkhamGame

      let patched = patch arkhamGameCurrentData (choicePatchDown $ arkhamStepChoice step)

      case patched of
        -- TODO: We need to add back the gameActionDiff
        -- ensure previous step exists
        Error e -> error $ T.pack e
        Success ge -> runDB do
          void $ select do
            game <- from $ table @ArkhamGame
            where_ $ game.id ==. val gameId
            locking forUpdate
          maybe (error $ "can not go back, at step: " <> tshow arkhamGameStep) (\_ -> pure ())
            =<< getBy (UniqueStep gameId (arkhamGameStep - 1))

          now <- liftIO getCurrentTime
          isDebug <- lookupGetParam "debug"

          seed <- liftIO getRandom
          let
            arkhamGame =
              case isDebug of
                Nothing ->
                  ArkhamGame
                    arkhamGameName
                    (ge {gameSeed = seed})
                    (arkhamGameStep - 1)
                    arkhamGameMultiplayerVariant
                    arkhamGameCreatedAt
                    now
                Just _ ->
                  ArkhamGame
                    arkhamGameName
                    ge
                    (arkhamGameStep - 1)
                    arkhamGameMultiplayerVariant
                    arkhamGameCreatedAt
                    now

          replace gameId arkhamGame
          delete do
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

putApiV1ArkhamGameUndoR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  game <- runDB $ get404 gameId
  ArkhamGame {..} <- stepBack userId gameId game
  writeChannel <- socketChannel <$> getRoom gameId

  atomically
    $ writeTChan writeChannel
    $ encode
    $ GameUpdate
    $ PublicGame gameId arkhamGameName [] arkhamGameCurrentData

putApiV1ArkhamGameUndoScenarioR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoScenarioR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  game <- runDB $ get404 gameId

  let n = gameScenarioSteps (arkhamGameCurrentData game) - 1

  ArkhamGame {..} <- stepBackN n userId gameId game

  x <- liftIO getRandom

  let ge = arkhamGameCurrentData {gameSeed = x}
  writeChannel <- socketChannel <$> getRoom gameId

  gameLog <- fmap (fmap unValue) . runDB $ select do
    entries <- from $ table @ArkhamLogEntry
    where_ $ entries.arkhamGameId ==. val gameId
    where_ $ entries.step <. val (arkhamGameStep - n)
    orderBy [desc entries.createdAt]
    pure entries.body

  now <- liftIO getCurrentTime

  runDB
    $ replace gameId
    $ ArkhamGame
      arkhamGameName
      ge
      arkhamGameStep
      arkhamGameMultiplayerVariant
      arkhamGameCreatedAt
      now

  atomically
    $ writeTChan writeChannel
    $ encode
    $ GameUpdate
    $ PublicGame gameId arkhamGameName gameLog ge

stepBackN :: Int -> UserId -> ArkhamGameId -> ArkhamGame -> Handler ArkhamGame
stepBackN n userId gameId ArkhamGame {..} = runDB do
  Entity pid arkhamPlayer <- getBy404 (UniquePlayer userId gameId)
  steps <- select do
    steps <- from $ table @ArkhamStep
    where_ $ steps.arkhamGameId ==. val gameId
    orderBy [desc steps.step]
    limit (fromIntegral n)
    pure steps

  void $ select do
    game <- from $ table @ArkhamGame
    where_ $ game.id ==. val gameId
    locking forUpdate

  update \g -> do
    set g [ArkhamGameStep =. val (arkhamGameStep - n)]
    where_ $ g.id ==. val gameId

  delete do
    xsteps <- from $ table @ArkhamStep
    where_ $ xsteps.id `in_` valList (map entityKey steps)

  delete do
    entries <- from $ table @ArkhamLogEntry
    where_ $ entries.arkhamGameId ==. val gameId
    where_ $ entries.step >=. val (arkhamGameStep - n)

  now <- liftIO getCurrentTime

  let undoPatch = foldMap (choicePatchDown . arkhamStepChoice . entityVal) steps

  case patch arkhamGameCurrentData undoPatch of
    Error e -> error $ T.pack e
    Success ge -> do
      let arkhamGame =
            ArkhamGame
              arkhamGameName
              ge
              (arkhamGameStep - n)
              arkhamGameMultiplayerVariant
              arkhamGameCreatedAt
              now

      replace gameId arkhamGame
      delete do
        entries <- from $ table @ArkhamLogEntry
        where_ $ entries.arkhamGameId ==. val gameId
        where_ $ entries.step >=. val (arkhamGameStep - 1)

      case arkhamGameMultiplayerVariant of
        Solo ->
          replace pid
            $ arkhamPlayer
              { arkhamPlayerInvestigatorId = coerce (view activeInvestigatorIdL ge)
              }
        WithFriends -> pure ()
      pure arkhamGame
