module Api.Handler.Arkham.Undo (putApiV1ArkhamGameUndoR, putApiV1ArkhamGameUndoScenarioR) where

import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Api.Handler.Arkham.Games.Shared (publishToRoom)
import Arkham.Card.CardCode
import Arkham.Game
import Arkham.Game.Diff
import Arkham.Id
import Control.Lens (view)
import Control.Monad.Except
import Control.Monad.Random (getRandom)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Patch
import Data.Text qualified as T
import Data.Time.Clock
import Database.Esqueleto.Experimental
import Entity.Arkham.GameRaw
import Entity.Arkham.LogEntry
import Entity.Arkham.Player
import Entity.Arkham.Step
import Import hiding (delete, on, update, (!=.), (<.), (=.), (==.), (>.), (>=.))
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

-- | Extract gameScenarioSteps from raw JSON without deserializing to Game.
getScenarioSteps :: Json.Value -> Int
getScenarioSteps (Object obj) =
  case KM.lookup "gameScenarioSteps" obj of
    Just (Number n) -> round n
    _ -> 0
getScenarioSteps _ = 0

-- | Single-step undo. Optimized to avoid the expensive Game<->Value round-trip:
-- fetches game state as raw JSON (ArkhamGameRaw), applies the patch at the Value
-- level, then deserializes to Game exactly once for the return value.
--
-- Old cost: fromJSON(fetch) + toJSON(patch) + fromJSON(patch) + toJSON(replace) = 4 conversions
-- New cost: fromJSON(return value only) = 1 conversion
stepBack :: Bool -> UserId -> ArkhamGameId -> DB (Either Json.Value ArkhamGame)
stepBack isDebug userId gameId = do
  lockGame gameId
  rawGame <- get404 (ArkhamGameRawKey gameId)
  runExceptT do
    Entity pid arkhamPlayer <- lift $ getBy404 (UniquePlayer userId gameId)
    let n = arkhamGameRawStep rawGame
    Entity stepId step <- maybeToExceptM (jsonError "Missing step") $ getBy (UniqueStep gameId n)
    -- never delete the initial step as it can not be redone
    -- NOTE: actually we never want to step back if the patchOperations are empty, the first condition is therefor redundant
    when (step.step <= 0) $ throwError $ jsonErrorContents step "Can't undo the first step"
    if null (patchOperations $ choicePatchDown step.choice)
      then do
        -- we don't need to apply any real updates so let's just remove the step
        -- ensure previous step exists
        maybeToExceptM_ (jsonError $ "can not go back, at step: " <> tshow n)
          $ getBy (UniqueStep gameId (n - 1))
        -- Parse once before DB changes (data unchanged, just decrement step)
        ge <- case fromJSON @Game rawGame.currentData of
          Error e -> throwError $ jsonError $ T.pack e
          Success g -> pure g
        lift do
          update \g -> do
            set g [ArkhamGameStep =. val (n - 1)]
            where_ $ g.id ==. val gameId
          delete do
            entries <- from $ table @ArkhamLogEntry
            where_ $ entries.arkhamGameId ==. val gameId
            where_ $ entries.step >=. val (n - 1)
          deleteKey stepId
        pure $ ArkhamGame rawGame.name ge (n - 1) rawGame.multiplayerVariant rawGame.createdAt rawGame.updatedAt
      else do
        case patchValueWithRecovery rawGame.currentData (choicePatchDown $ arkhamStepChoice step) of
          -- TODO: We need to add back the gameActionDiff
          -- ensure previous step exists
          Error e -> throwError $ jsonError $ T.pack e
          Success patchedValue -> do
            maybeToExceptM_ (jsonError $ "can not go back, at step: " <> tshow n)
              $ getBy (UniqueStep gameId (n - 1))

            now <- liftIO getCurrentTime
            seed <- liftIO getRandom

            let finalValue = if isDebug then patchedValue else setGameSeed seed patchedValue

            -- Deserialize exactly once for the return value (PublicGame + Solo mode)
            ge <- case fromJSON @Game finalValue of
              Error e -> throwError $ jsonError $ T.pack e
              Success g -> pure g

            let
              arkhamGame =
                ArkhamGame
                  rawGame.name
                  ge
                  (n - 1)
                  rawGame.multiplayerVariant
                  rawGame.createdAt
                  now
            lift do
              -- Store raw Value directly, avoiding toJSON :: Game -> Value
              replace (ArkhamGameRawKey gameId) $
                ArkhamGameRaw
                  rawGame.name
                  finalValue
                  (n - 1)
                  rawGame.multiplayerVariant
                  rawGame.createdAt
                  now
              delete do
                entries <- from $ table @ArkhamLogEntry
                where_ $ entries.arkhamGameId ==. val gameId
                where_ $ entries.step >=. val (n - 1)
              deleteKey stepId

              case rawGame.multiplayerVariant of
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
  userId <- runDB do
    getBy (UniquePlayer userId' gameId) >>= \case
      Nothing | user.admin -> do
        game <- get404 gameId
        player <- get404 @_ @_ @ArkhamPlayer $ coerce $ gameActivePlayerId game.currentData
        pure player.userId
      _ -> pure userId'
  -- NOTE: do not call sendStatusJSON inside withSpan_. withSpan_'s bracket-style
  -- exception handling rewraps Yesod's HCContent control-flow exception in an
  -- AnnotatedException, which Yesod cannot unwrap, so the response degrades to
  -- a generic 500 and the actual error JSON is lost.
  result <- withSpan_ "stepBack" $ runDB (stepBack isDebug userId gameId)
  case result of
    Left err -> do
      liftIO $ print err
      sendStatusJSON Status.status400 err
    Right (ArkhamGame {..}) -> do
      publishToRoom gameId
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
            where_ $ entries.step <. val (agame.step - n)
            orderBy [desc entries.createdAt]
            pure entries.body

        let g =
              ArkhamGame
                agame.name
                (agame.currentData {gameSeed = x})
                agame.step
                agame.multiplayerVariant
                agame.createdAt
                now

        replace gameId g
        pure (g, gameLog)

  case eResult of
    Left err -> do
      liftIO $ print err
      sendStatusJSON Status.status400 err
    Right (ArkhamGame {..}, gameLog) -> do
      publishToRoom gameId
        $ GameUpdate
        $ PublicGame gameId arkhamGameName gameLog arkhamGameCurrentData

-- | Multi-step scenario undo. Like stepBack but optimized to apply a combined
-- patch at the Value level and deserialize only once for the return value.
--
-- Old cost: fromJSON(fetch) + toJSON(patch) + fromJSON(patch) + toJSON(replace in func) = 4 conversions
-- New cost: fromJSON(return value only) = 1 conversion (handler's replace adds 1 more)
stepBackScenario :: UserId -> ArkhamGameId -> DB (Either Json.Value (ArkhamGame, Int))
stepBackScenario userId gameId = do
  lockGame gameId
  rawGame <- get404 (ArkhamGameRawKey gameId)
  runExceptT do
    let n = getScenarioSteps rawGame.currentData - 1
    when (n <= 0) $ throwError "No scenario steps to undo"
    Entity pid arkhamPlayer <- lift $ getBy404 (UniquePlayer userId gameId)
    let toStep = max 0 (arkhamGameRawStep rawGame - n)
    steps <- lift $ select do
      steps <- from $ table @ArkhamStep
      where_ $ steps.arkhamGameId ==. val gameId
      orderBy [desc steps.step]
      limit (fromIntegral n)
      where_ $ steps.step !=. val 0
      pure steps

    lift do
      delete do
        xsteps <- from $ table @ArkhamStep
        where_ $ xsteps.id `in_` valList (map entityKey steps)

      delete do
        entries <- from $ table @ArkhamLogEntry
        where_ $ entries.arkhamGameId ==. val gameId
        where_ $ entries.step >. val toStep

    now <- liftIO getCurrentTime

    let undoPatch = foldMap (choicePatchDown . arkhamStepChoice . entityVal) steps

    case patchValueWithRecovery rawGame.currentData undoPatch of
      Error e -> throwError $ jsonError $ T.pack e
      Success patchedValue -> do
        -- Deserialize exactly once for the return value
        ge <- case fromJSON @Game patchedValue of
          Error e -> throwError $ jsonError $ T.pack e
          Success g -> pure g

        let arkhamGame = ArkhamGame rawGame.name ge toStep rawGame.multiplayerVariant rawGame.createdAt now

        lift do
          -- Store raw Value directly, avoiding toJSON :: Game -> Value
          -- Note: the handler will replace again with an updated gameSeed
          replace (ArkhamGameRawKey gameId) $
            ArkhamGameRaw
              rawGame.name
              patchedValue
              toStep
              rawGame.multiplayerVariant
              rawGame.createdAt
              now

          case rawGame.multiplayerVariant of
            Solo ->
              replace pid
                $ arkhamPlayer
                  { arkhamPlayerInvestigatorId = coerce (view activeInvestigatorIdL ge)
                  }
            WithFriends -> pure ()
          pure (arkhamGame, n)
