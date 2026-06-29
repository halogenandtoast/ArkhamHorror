module Api.Handler.Arkham.Undo (
  putApiV1ArkhamGameUndoR,
  putApiV1ArkhamGameUndoScenarioR,
  putApiV1ArkhamGameUndoActionR,
  putApiV1ArkhamGameUndoTurnR,
  putApiV1ArkhamGameUndoPhaseR,
  putApiV1ArkhamGameUndoRoundR,
) where

import Api.Arkham.Epic (getGameUndoFloor, lookupGameEvent, revertEpicDeltasForGameStep)
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Api.Handler.Arkham.Games.Shared (propagateShared, publishToRoom)
import Arkham.Epic.Types (SharedEventState)
import Arkham.Card.CardCode
import Arkham.Game
import Arkham.Game.Diff
import Arkham.Id
import Control.Lens (view)
import Control.Monad (foldM)
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

{- | Extract a top-level optional Int field from raw JSON without
deserializing to Game. Used to read undo boundary fields.
-}
getMaybeIntField :: Json.Key -> Json.Value -> Maybe Int
getMaybeIntField field (Object obj) =
  case KM.lookup field obj of
    Just (Number n) -> Just (round n)
    _ -> Nothing
getMaybeIntField _ _ = Nothing

{- | Single-step undo. Optimized to avoid the expensive Game<->Value round-trip:
fetches game state as raw JSON (ArkhamGameRaw), applies the patch at the Value
level, then deserializes to Game exactly once for the return value.

Old cost: fromJSON(fetch) + toJSON(patch) + fromJSON(patch) + toJSON(replace) = 4 conversions
New cost: fromJSON(return value only) = 1 conversion
-}
stepBack
  :: Bool
  -> UserId
  -> ArkhamGameId
  -> DB (Either Json.Value (ArkhamGame, Maybe (ArkhamEpicEventId, SharedEventState)))
stepBack isDebug userId gameId = do
  lockGame gameId
  rawGame <- get404 (ArkhamGameRawKey gameId)
  runExceptT do
    Entity pid arkhamPlayer <- lift $ getBy404 (UniquePlayer userId gameId)
    let n = arkhamGameRawStep rawGame
    -- Epic Multiplayer: if this game is a group within an event, revert any
    -- shared-counter deltas this step recorded (additive deltas commute, so this
    -- is correct even if other groups moved the counter since) and return the
    -- restored shared state + event id so the caller can propagate it to the
    -- OTHER groups post-commit (so e.g. an undone countermeasure/blob-health spend
    -- is restored on every group's board immediately).
    mEvent <- lift $ lookupGameEvent gameId
    let revertShared :: DB (Maybe (ArkhamEpicEventId, SharedEventState))
        revertShared = case mEvent of
          Nothing -> pure Nothing
          Just (eventEntity, _) -> do
            let eid = entityKey eventEntity
            mShared <- revertEpicDeltasForGameStep eid gameId n
            pure $ (\s -> (eid, s)) <$> mShared
    -- Epic Multiplayer: the per-game undo FLOOR walls off undo across a cross-group
    -- act-clue advance (clues spent + acts flipped in OTHER groups have no sound
    -- local inverse). 0 = no floor (every non-epic game).
    undoFloor <- lift $ getGameUndoFloor gameId
    Entity stepId step <- maybeToExceptM (jsonError "Missing step") $ getBy (UniqueStep gameId n)
    -- never delete the initial step as it can not be redone
    -- NOTE: actually we never want to step back if the patchOperations are empty, the first condition is therefor redundant
    when (step.step <= 0) $ throwError $ jsonErrorContents step "Can't undo the first step"
    when (undoFloor > 0 && step.step <= undoFloor)
      $ throwError
      $ jsonError "Can't undo past the act advance"
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
        plan <- lift do
          update \g -> do
            set g [ArkhamGameStep =. val (n - 1)]
            where_ $ g.id ==. val gameId
          delete do
            entries <- from $ table @ArkhamLogEntry
            where_ $ entries.arkhamGameId ==. val gameId
            where_ $ entries.step >=. val (n - 1)
          deleteKey stepId
          revertShared
        pure
          ( ArkhamGame rawGame.name ge (n - 1) rawGame.multiplayerVariant rawGame.createdAt rawGame.updatedAt
          , plan
          )
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
              replace (ArkhamGameRawKey gameId)
                $ ArkhamGameRaw
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
              plan <- revertShared

              case rawGame.multiplayerVariant of
                Solo ->
                  replace pid
                    $ arkhamPlayer
                      { arkhamPlayerInvestigatorId = coerce (view activeInvestigatorIdL ge)
                      }
                WithFriends -> pure ()
              pure (arkhamGame, plan)

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
    Right (ArkhamGame {..}, mPropagate) -> do
      publishToRoom gameId
        $ GameUpdate
        $ PublicGame gameId arkhamGameName [] arkhamGameCurrentData
      -- Epic Multiplayer: if this undo reverted shared-counter deltas (a
      -- commutative counter like countermeasures / blob health), propagate the
      -- restored shared state across the WHOLE event POST-COMMIT (outside the game
      -- lock, so we can take the other groups' locks without a game->game
      -- deadlock): broadcast it and re-sync each OTHER group's board so the
      -- restored value is available everywhere immediately, not on their next
      -- action. The origin game already reflects the revert via its own patch, so
      -- it is skipped.
      for_ mPropagate \(eid, shared) -> propagateShared eid (Just gameId) shared

putApiV1ArkhamGameUndoScenarioR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoScenarioR =
  multiStepUndoHandler "stepBackN" stepBackScenario

putApiV1ArkhamGameUndoActionR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoActionR =
  multiStepUndoHandler "stepBackAction" (stepBackToBoundary "gameUndoActionStep")

putApiV1ArkhamGameUndoTurnR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoTurnR =
  multiStepUndoHandler "stepBackTurn" (stepBackToBoundary "gameUndoTurnStep")

putApiV1ArkhamGameUndoPhaseR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoPhaseR =
  multiStepUndoHandler "stepBackPhase" (stepBackToBoundary "gameUndoPhaseStep")

putApiV1ArkhamGameUndoRoundR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameUndoRoundR =
  multiStepUndoHandler "stepBackRound" (stepBackToBoundary "gameUndoRoundStep")

{- | Shared handler logic for multi-step undo endpoints. Reseeds the game,
replaces the row with an updated `updatedAt`, and rebroadcasts the
truncated game log.
-}
multiStepUndoHandler
  :: ByteString
  -> (UserId -> ArkhamGameId -> DB (Either Json.Value (ArkhamGame, Maybe (ArkhamEpicEventId, SharedEventState))))
  -> ArkhamGameId
  -> Handler ()
multiStepUndoHandler spanName runStepBack gameId = do
  userId <- getRequestUserId
  x <- liftIO getRandom
  now <- liftIO getCurrentTime
  eResult <- withSpan_ spanName $ runDB do
    runExceptT do
      (agame, mPropagate) <- ExceptT $ runStepBack userId gameId
      lift do
        gameLog :: [Text] <-
          fmap unValue <$> select do
            entries <- from $ table @ArkhamLogEntry
            where_ $ entries.arkhamGameId ==. val gameId
            -- After landing at agame.step the surviving log entries are exactly those
            -- tagged < agame.step (the log delete drops >= toStep == agame.step).
            where_ $ entries.step <. val agame.step
            -- Order by step (monotonic per game) so the planner can use
            -- idx_arkham_log_entry_gameid_step directly. id (bigserial)
            -- is the within-step tiebreaker.
            orderBy [desc entries.step, desc entries.id]
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
        pure (g, gameLog, mPropagate)

  case eResult of
    Left err -> do
      liftIO $ print err
      sendStatusJSON Status.status400 err
    Right (ArkhamGame {..}, gameLog, mPropagate) -> do
      publishToRoom gameId
        $ GameUpdate
        $ PublicGame gameId arkhamGameName gameLog arkhamGameCurrentData
      -- Epic Multiplayer: propagate the restored shared state across the event
      -- post-commit (outside the game lock, so other groups' locks are safe to
      -- take), mirroring single-step undo. The origin already reflects the revert
      -- via its own patch, so it is skipped.
      for_ mPropagate \(eid, shared) -> propagateShared eid (Just gameId) shared

-- | Multi-step scenario undo: roll back to scenarioSteps = 1 (start of scenario).
stepBackScenario :: UserId -> ArkhamGameId -> DB (Either Json.Value (ArkhamGame, Maybe (ArkhamEpicEventId, SharedEventState)))
stepBackScenario userId gameId = do
  lockGame gameId
  rawGame <- get404 (ArkhamGameRawKey gameId)
  stepBackToScenarioStep userId gameId rawGame 1

{- | Multi-step undo to a boundary recorded in the raw JSON (e.g.
"undoActionStep"). Errors out if the boundary is not set.
-}
stepBackToBoundary
  :: Json.Key
  -> UserId
  -> ArkhamGameId
  -> DB (Either Json.Value (ArkhamGame, Maybe (ArkhamEpicEventId, SharedEventState)))
stepBackToBoundary field userId gameId = do
  lockGame gameId
  rawGame <- get404 (ArkhamGameRawKey gameId)
  case getMaybeIntField field rawGame.currentData of
    Nothing -> pure $ Left $ jsonError $ "No boundary set for " <> tshow field
    Just target -> stepBackToScenarioStep userId gameId rawGame target

{- | Multi-step undo to the given target scenarioSteps value. Caller is
responsible for having locked the game and fetched the raw game state.

Optimized to apply a combined patch at the Value level and deserialize only
once for the return value:
  Old cost: fromJSON(fetch) + toJSON(patch) + fromJSON(patch) + toJSON(replace) = 4
  New cost: fromJSON(return value only) = 1
-}
stepBackToScenarioStep
  :: UserId
  -> ArkhamGameId
  -> ArkhamGameRaw
  -> Int
  -> DB (Either Json.Value (ArkhamGame, Maybe (ArkhamEpicEventId, SharedEventState)))
stepBackToScenarioStep userId gameId rawGame targetStep = runExceptT do
  let currentSteps = getScenarioSteps rawGame.currentData
      n = currentSteps - targetStep
  when (n <= 0) $ throwError "Nothing to undo"
  Entity pid arkhamPlayer <- lift $ getBy404 (UniquePlayer userId gameId)
  -- Epic Multiplayer: never let a multi-step undo cross the act-advance floor
  -- (see 'stepBack'); clamp the target up to it. 0 = no floor.
  undoFloor <- lift $ getGameUndoFloor gameId
  let toStep = max undoFloor (arkhamGameRawStep rawGame - n)
  -- Select the steps to revert as those ABOVE the (possibly floor-clamped) target
  -- rather than the top @n@: identical to @limit n@ when unclamped (steps are
  -- contiguous, so @step > rawStep - n@ is exactly the newest n), but when the
  -- floor clamps @toStep@ up it correctly reverts ONLY the steps above the floor,
  -- keeping the reverted patch, the persisted steps, and @toStep@ consistent.
  steps <- lift $ select do
    steps <- from $ table @ArkhamStep
    where_ $ steps.arkhamGameId ==. val gameId
    where_ $ steps.step >. val toStep
    where_ $ steps.step !=. val 0
    orderBy [desc steps.step]
    pure steps

  lift do
    -- The step-deletion trigger only permits deleting steps greater than the
    -- game's current step. Move the game cursor first, then trim future steps.
    update \g -> do
      set g [ArkhamGameStep =. val toStep]
      where_ $ g.id ==. val gameId

    -- Range delete by (game_id, step) instead of materializing every step's
    -- UUID into an IN(...) list. Same row set, but the planner uses a single
    -- index scan on steps_game_step_idx and there's no client->server
    -- round-trip of N UUIDs.
    delete do
      xsteps <- from $ table @ArkhamStep
      where_ $ xsteps.arkhamGameId ==. val gameId
      where_ $ xsteps.step >. val toStep

    -- Log entries are tagged ONE LOWER than their ArkhamStep (the action landing at
    -- ArkhamStep k logs under step k-1), so the log delete must be >= toStep to also
    -- drop the log of the first undone action (the one that landed at toStep+1), not
    -- just > toStep.
    delete do
      entries <- from $ table @ArkhamLogEntry
      where_ $ entries.arkhamGameId ==. val gameId
      where_ $ entries.step >=. val toStep

  -- Epic Multiplayer: multi-step undo must ALSO revert the shared-counter deltas
  -- recorded on every undone step (single-step 'stepBack' already does), deleting
  -- their ArkhamEpicStep rows — otherwise the shared state goes stale and those rows
  -- orphan into a later double-revert. Fold the per-step reverts (each re-reads the
  -- locked event row, so they compound) into one restored state to propagate
  -- post-commit. Every undone step is above the floor, so reverting it is correct.
  mEvent <- lift $ lookupGameEvent gameId
  mPropagate <- lift $ case mEvent of
    Nothing -> pure Nothing
    Just (eventEntity, _) -> do
      let eid = entityKey eventEntity
      mShared <-
        foldM
          (\acc st -> (<|> acc) <$> revertEpicDeltasForGameStep eid gameId ((entityVal st).step))
          Nothing
          steps
      pure $ (\s -> (eid, s)) <$> mShared

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
        replace (ArkhamGameRawKey gameId)
          $ ArkhamGameRaw
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
        pure (arkhamGame, mPropagate)
