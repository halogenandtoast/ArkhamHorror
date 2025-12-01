module Api.Handler.Arkham.Undo (putApiV1ArkhamGameUndoR, putApiV1ArkhamGameUndoScenarioR) where

import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Card.CardCode
import Arkham.Game
import Arkham.Game.Diff
import Arkham.Id
import Control.Lens (view)
import Control.Monad.Except
import Control.Monad.Random (getRandom)
import Data.Aeson.Patch
import Data.Text qualified as T
import Data.Time.Clock
import Database.Esqueleto.Experimental
import Entity.Arkham.LogEntry
import Entity.Arkham.Player
import Entity.Arkham.Step
import Import hiding (delete, on, update, (!=.), (<.), (=.), (==.), (>=.), (>.))
import Json
import Network.HTTP.Types.Status qualified as Status
import OpenTelemetry.Eventlog (withSpan_)

jsonError :: Text -> Json.Value
jsonError msg = object ["error" .= msg]

jsonErrorContents :: ToJSON v => v -> Text -> Json.Value
jsonErrorContents v msg = object ["error" .= msg, "contents" .= v]

maybeToExcept :: Monad m => e -> Maybe a -> ExceptT e m a
maybeToExcept msg Nothing = throwError msg
maybeToExcept _ (Just a) = pure a

maybeToExceptM :: Monad m => e -> m (Maybe a) -> ExceptT e m a
maybeToExceptM msg ma = do
  a <- lift ma
  maybeToExcept msg a

maybeToExceptM_ :: Monad m => e -> m (Maybe a) -> ExceptT e m ()
maybeToExceptM_ msg ma = do
  a <- lift ma
  void $ maybeToExcept msg a

stepBack :: Bool -> UserId -> ArkhamGameId -> DB (Either Json.Value ArkhamGame)
stepBack isDebug userId gameId = atomicallyWithGame gameId \game ->
  runExceptT do
    Entity pid arkhamPlayer <- lift $ getBy404 (UniquePlayer userId gameId)
    let n = arkhamGameStep game
    Entity stepId step <- maybeToExceptM (jsonError "Missing step") $ getBy (UniqueStep gameId n)
    -- never delete the initial step as it can not be redone
    -- NOTE: actually we never want to step back if the patchOperations are empty, the first condition is therefor redundant
    when (arkhamStepStep step <= 0) $ throwError $ jsonErrorContents step "Can't undo the first step"
    if null (patchOperations $ choicePatchDown $ arkhamStepChoice step)
      then do
        -- we don't need to apply any real updates so let's just remove the step
        -- ensure previous step exists
        maybeToExceptM_ (jsonError $ "can not go back, at step: " <> tshow n)
          $ getBy (UniqueStep gameId (n - 1))
        lift do
          update \g -> do
            set g [ArkhamGameStep =. val (n - 1)]
            where_ $ g.id ==. val gameId
          delete do
            entries <- from $ table @ArkhamLogEntry
            where_ $ entries.arkhamGameId ==. val gameId
            where_ $ entries.step >=. val (n - 1)
          deleteKey stepId
          pure $ game {arkhamGameStep = n - 1}
      else do
        case patch (arkhamGameCurrentData game) (choicePatchDown $ arkhamStepChoice step) of
          -- TODO: We need to add back the gameActionDiff
          -- ensure previous step exists
          Error e -> throwError $ jsonError $ T.pack e
          Success ge -> do
            maybeToExceptM_ (jsonError $ "can not go back, at step: " <> tshow n)
              $ getBy (UniqueStep gameId (n - 1))

            now <- liftIO getCurrentTime
            seed <- liftIO getRandom

            let
              arkhamGame =
                ArkhamGame
                  (arkhamGameName game)
                  (if isDebug then ge else ge {gameSeed = seed})
                  (n - 1)
                  (arkhamGameMultiplayerVariant game)
                  (arkhamGameCreatedAt game)
                  now
            lift do
              replace gameId arkhamGame
              delete do
                entries <- from $ table @ArkhamLogEntry
                where_ $ entries.arkhamGameId ==. val gameId
                where_ $ entries.step >=. val (n - 1)
              deleteKey stepId

              case arkhamGameMultiplayerVariant game of
                Solo ->
                  replace pid
                    $ arkhamPlayer
                      { arkhamPlayerInvestigatorId = coerce (view activeInvestigatorIdL ge)
                      }
                WithFriends -> pure ()
              pure arkhamGame

putApiV1ArkhamGameUndoR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoR gameId = do
  Entity userId' user <- getRequestUser
  isDebug <- isJust <$> lookupGetParam "debug"
  mPlayer <- runDB $ Import.exists (UniquePlayer userId' gameId)
  userId <- case mPlayer of
    Just _ -> pure userId'
    Nothing | user.admin -> do
      player <- runDB $ get404 $ coerce $ gameActivePlayerId $ arkhamGameCurrentData game
      pure $ coerce $ arkhamPlayerUserId player
  withSpan_ "stepBack" do
    runDB (stepBack isDebug userId gameId) >>= \case
      Left err -> sendStatusJSON Status.status400 err
      Right (ArkhamGame {..}) -> do
        writeChannel <- socketChannel <$> getRoom gameId
        atomically
          $ writeTChan writeChannel
          $ encode
          $ GameUpdate
          $ PublicGame gameId arkhamGameName [] arkhamGameCurrentData

putApiV1ArkhamGameUndoScenarioR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoScenarioR gameId = do
  userId <- getRequestUserId
  x <- liftIO getRandom
  now <- liftIO getCurrentTime
  eResult <- withSpan_ "stepBackN" $ runDB do
    runExceptT do
      (agame, n) <- ExceptT $ stepBackScenario userId gameId
      lift do
        gameLog :: [Text] <-
          fmap unValue <$> select do
            entries <- from $ table @ArkhamLogEntry
            where_ $ entries.arkhamGameId ==. val gameId
            where_ $ entries.step <. val (arkhamGameStep agame - n)
            orderBy [desc entries.createdAt]
            pure entries.body

        let g =
              ArkhamGame
                (arkhamGameName agame)
                ((arkhamGameCurrentData agame) {gameSeed = x})
                (arkhamGameStep agame)
                (arkhamGameMultiplayerVariant agame)
                (arkhamGameCreatedAt agame)
                now

        replace gameId g
        pure (g, gameLog)

  case eResult of
    Left err -> sendStatusJSON Status.status400 err
    Right (ArkhamGame {..}, gameLog) -> do
      writeChannel <- socketChannel <$> getRoom gameId

      atomically
        $ writeTChan writeChannel
        $ encode
        $ GameUpdate
        $ PublicGame gameId arkhamGameName gameLog arkhamGameCurrentData

stepBackScenario :: UserId -> ArkhamGameId -> DB (Either Json.Value (ArkhamGame, Int))
stepBackScenario userId gameId = atomicallyWithGame gameId \game ->
  runExceptT do
    let n = gameScenarioSteps (arkhamGameCurrentData game) - 1
    when (n <= 0) $ throwError "No scenario steps to undo"
    Entity pid arkhamPlayer <- lift $ getBy404 (UniquePlayer userId gameId)
    let toStep = max 0 (arkhamGameStep game - n)
    steps <- lift $ select do
      steps <- from $ table @ArkhamStep
      where_ $ steps.arkhamGameId ==. val gameId
      orderBy [desc steps.step]
      limit (fromIntegral n)
      where_ $ steps.step !=. val 0
      pure steps

    lift do
      update \g -> do
        set g [ArkhamGameStep =. val toStep]
        where_ $ g.id ==. val gameId

      delete do
        xsteps <- from $ table @ArkhamStep
        where_ $ xsteps.id `in_` valList (map entityKey steps)

      delete do
        entries <- from $ table @ArkhamLogEntry
        where_ $ entries.arkhamGameId ==. val gameId
        where_ $ entries.step >. val toStep

    now <- liftIO getCurrentTime

    let undoPatch = foldMap (choicePatchDown . arkhamStepChoice . entityVal) steps

    case patch (arkhamGameCurrentData game) undoPatch of
      Error e -> throwError $ jsonError $ T.pack e
      Success ge -> lift do
        let arkhamGame =
              ArkhamGame
                (arkhamGameName game)
                ge
                toStep
                (arkhamGameMultiplayerVariant game)
                (arkhamGameCreatedAt game)
                now

        replace gameId arkhamGame
        delete do
          entries <- from $ table @ArkhamLogEntry
          where_ $ entries.arkhamGameId ==. val gameId
          where_ $ entries.step >. val toStep

        case arkhamGameMultiplayerVariant game of
          Solo ->
            replace pid
              $ arkhamPlayer
                { arkhamPlayerInvestigatorId = coerce (view activeInvestigatorIdL ge)
                }
          WithFriends -> pure ()
        pure (arkhamGame, n)
