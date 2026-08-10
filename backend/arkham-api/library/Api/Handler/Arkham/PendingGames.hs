module Api.Handler.Arkham.PendingGames (
  getApiV1ArkhamPendingGameR,
  putApiV1ArkhamPendingGameR,
) where

import Import hiding (on, (==.))

import Api.Arkham.Epic (applyEpicDeltasLocked, lookupGameEvent, mkEpicEnv)
import Api.Arkham.Helpers
import Api.Handler.Arkham.Games.Shared (
  broadcastEventChanged,
  epicSyncMessages,
  propagateShared,
  publishToRoom,
 )
import Arkham.Classes.HasQueue
import Arkham.Epic.Types (
  EpicRole (GroupPlayer),
  GroupOrdinal (..),
  epicEnvDeltaRef,
  epicEnvGroup,
  epicEnvSharedRef,
 )
import Arkham.Game
import Arkham.Game.State
import Arkham.Id
import Arkham.Queue
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Data.Time.Clock
import Database.Persist ((==.))
import Entity.Arkham.Step

getApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (PublicGame ArkhamGameId)
getApiV1ArkhamPendingGameR gameId = do
  _ <- getRequestUserId
  ge <- runDB $ get404 gameId
  pure $ toPublicGame (Entity gameId ge) mempty

putApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (PublicGame ArkhamGameId)
putApiV1ArkhamPendingGameR gameId = do
  userId <- getRequestUserId
  now <- liftIO getCurrentTime
  -- A player may occupy seats in only one group of an event. The normal UI
  -- enforces this too, but direct invite URLs/API calls must not be able to move
  -- the membership row while leaving ArkhamPlayer rows in multiple games.
  mJoinEvent <- runDB $ lookupGameEvent gameId
  for_ mJoinEvent \(eventEntity, GroupOrdinal requestedOrdinal) -> do
    mMembership <-
      runDB
        $ selectFirst
          [ ArkhamEpicMemberArkhamEpicEventId ==. entityKey eventEntity
          , ArkhamEpicMemberUserId ==. userId
          , ArkhamEpicMemberRole ==. GroupPlayer
          ]
          []
    for_ mMembership \(Entity _ membership) ->
      when (arkhamEpicMemberGroupOrdinal membership /= Just requestedOrdinal)
        $ permissionDenied "You already occupy a seat in another group in this event"
  (game@ArkhamGame {..}, mShared) <- runDB $ atomicallyWithGame gameId \original@ArkhamGame {..} -> do
    case gameGameState arkhamGameCurrentData of
      IsPending _ -> do
        alreadyExists <- exists [ArkhamPlayerArkhamGameId ==. gameId, ArkhamPlayerUserId ==. userId]

        if alreadyExists
          then pure (original, Nothing)
          else do
            mLastStep <- getBy (UniqueStep gameId arkhamGameStep)
            let currentQueue = maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

            gameRef <- liftIO $ newIORef arkhamGameCurrentData
            queueRef <- liftIO $ newQueue currentQueue
            genRef <- liftIO $ newIORef (mkStdGen (gameSeed arkhamGameCurrentData))

            pid <- insert $ ArkhamPlayer userId gameId "00000"

            -- Epic Multiplayer: discover the event (if any). Joining enrolls the
            -- user as a member, and setup must run event-aware so the group's
            -- shared values (countermeasures, Subject 8L-08 health) start from
            -- the CURRENT pool rather than 0/seed — otherwise a group set up
            -- after others have acted would not reflect their spends/damage.
            mEpicCtx <- lookupGameEvent gameId
            for_ mEpicCtx \(eventEntity, GroupOrdinal groupOrd) -> do
              membership <-
                insertBy
                  $ ArkhamEpicMember (entityKey eventEntity) userId GroupPlayer (Just groupOrd)
              case membership of
                Left (Entity _ existing)
                  | arkhamEpicMemberGroupOrdinal existing /= Just groupOrd ->
                      error "player already belongs to another event group"
                _ -> pure ()
            mEpicEnv <- traverse (uncurry mkEpicEnv) mEpicCtx

            runGameApp (GameApp gameRef queueRef genRef (pure . const ()) mEpicEnv) $ do
              addPlayer (PlayerId $ coerce pid)
              -- Run setup. For a multiplayer lobby this PAUSES at ChooseDeck
              -- (IsChooseDecks) until players pick decks, so we must NOT run any
              -- further messages here or we'd blast past deck selection and start
              -- the scenario with zero investigators ("No lead found").
              runMessages (gameIdToText gameId) Nothing
              -- Only when setup actually completed in this request (e.g. a fully
              -- pre-decked/AI group) do we reconcile the board to the shared pool.
              -- Otherwise the reconcile happens after deck selection via the
              -- action pull (StartScenario preserves EpicShared counts) + push.
              setupState <- gameGameState <$> liftIO (readIORef gameRef)
              when (setupState == IsActive) $ for_ mEpicEnv \epic -> do
                shared <- liftIO $ readIORef (epicEnvSharedRef epic)
                pushAll (epicSyncMessages (epicEnvGroup epic) shared)
                runMessages (gameIdToText gameId) Nothing

            updatedGame <- liftIO $ readIORef gameRef
            updatedQueue <- liftIO $ readIORef (queueToRef queueRef)

            -- Commit any shared deltas emitted during setup (same transaction).
            mShared <- case (mEpicCtx, mEpicEnv) of
              (Just (eventEntity, _), Just epic) -> do
                deltas <- liftIO $ readIORef (epicEnvDeltaRef epic)
                if null deltas
                  then pure Nothing
                  else do
                    s <- applyEpicDeltasLocked (entityKey eventEntity) (Just gameId) (Just (arkhamGameStep + 1)) deltas
                    pure $ Just (entityKey eventEntity, s)
              _ -> pure Nothing

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

            pure (game', mShared)
      _ -> pure (original, Nothing)

  for_ mShared \(eid, s) -> propagateShared eid (Just gameId) s
  for_ mJoinEvent \(eventEntity, _) -> broadcastEventChanged (entityKey eventEntity)
  publishToRoom gameId
    $ GameUpdate
    $ PublicGame gameId arkhamGameName [] arkhamGameCurrentData
  pure $ toPublicGame (Entity gameId game) mempty
