{-# LANGUAGE OverloadedRecordDot #-}

-- | Server-side orchestration for Epic Multiplayer shared state.
--
-- The authoritative shared state lives in a single @arkham_epic_events@ row.
-- A group's engine emits invertible 'SharedDelta's (captured during its action,
-- see 'Arkham.Game.captureSharedDelta'); this module applies them to the locked
-- event row and records each as an 'ArkhamEpicStep' so the mutation can be
-- reverted on undo.
module Api.Arkham.Epic where

import Arkham.Epic.Types
import Data.Time.Clock (getCurrentTime)
import Database.Esqueleto.Experimental hiding (update, (=.))
import Entity.Arkham.Epic
import Import hiding (on, (==.))
import Import qualified as P

-- | Find the event and this game's group ordinal, if the game is part of an
-- event. Cheap indexed lookup on the @arkham_epic_groups@ join table, so the
-- central @arkham_games@ table needs no event columns.
lookupGameEvent
  :: MonadIO m
  => ArkhamGameId
  -> ReaderT SqlBackend m (Maybe (Entity ArkhamEpicEvent, GroupOrdinal))
lookupGameEvent gameId = do
  rows <- select do
    (grp :& evt) <-
      from
        $ table @ArkhamEpicGroup
          `innerJoin` table @ArkhamEpicEvent
        `on` (\(grp :& evt) -> grp.arkhamEpicEventId ==. evt.id)
    where_ $ grp.arkhamGameId ==. just (val gameId)
    pure (evt, grp.ordinal)
  pure $ case rows of
    (evt, Value ordinal) : _ -> Just (evt, GroupOrdinal ordinal)
    [] -> Nothing

-- | Build a per-action 'EpicEnv': the current shared state in an 'IORef' plus an
-- empty delta buffer that the run loop appends to.
mkEpicEnv
  :: MonadIO m => Entity ArkhamEpicEvent -> GroupOrdinal -> ReaderT SqlBackend m EpicEnv
mkEpicEnv (Entity eid e) ordinal = do
  sharedRef <- liftIO $ newIORef (arkhamEpicEventSharedState e)
  deltaRef <- liftIO $ newIORef []
  pure
    EpicEnv
      { epicEnvId = coerce eid
      , epicEnvGroup = ordinal
      , epicEnvSharedRef = sharedRef
      , epicEnvDeltaRef = deltaRef
      }

-- | Apply a batch of deltas under a @FOR UPDATE@ lock on the event row, persist
-- the new shared state, and record one 'ArkhamEpicStep' per delta. Returns the
-- new shared state. Lock order is always game-then-event (callers already hold
-- the game lock), and the lock is held only for this short critical section.
applyEpicDeltasLocked
  :: MonadIO m
  => ArkhamEpicEventId
  -> Maybe ArkhamGameId
  -> Maybe Int
  -> [SharedDelta]
  -> ReaderT SqlBackend m SharedEventState
applyEpicDeltasLocked eid mGameId mGameStep deltas = do
  locked <- select do
    e <- from $ table @ArkhamEpicEvent
    where_ $ e.id ==. val eid
    locking forUpdate
    pure e
  case locked of
    [] -> error "applyEpicDeltasLocked: epic event row vanished mid-transaction"
    (Entity _ e : _) -> do
      now <- liftIO getCurrentTime
      let s0 = arkhamEpicEventSharedState e
          baseStep = arkhamEpicEventStep e
          s1 = foldl' (flip applyDelta) s0 deltas
      P.update
        eid
        [ ArkhamEpicEventSharedState P.=. s1
        , ArkhamEpicEventStep P.=. baseStep + length deltas
        , ArkhamEpicEventUpdatedAt P.=. now
        ]
      for_ (zip [1 ..] deltas) \(i, d) ->
        insert_ $ ArkhamEpicStep eid (baseStep + i) mGameId mGameStep d now
      pure s1

-- | Apply a pure update to the event's shared state under a @FOR UPDATE@ lock and
-- persist it, returning the new state. For barrier/timer BOOKKEEPING
-- (groups-ready bitmask, timer-start) that is set directly rather than as an
-- undoable additive delta — so it records no 'ArkhamEpicStep'. The update runs
-- inside the lock, so concurrent callers see each other's writes (e.g. two groups
-- marking ready at once can't lose a bit).
modifySharedStateLocked
  :: MonadIO m
  => ArkhamEpicEventId
  -> (SharedEventState -> SharedEventState)
  -> ReaderT SqlBackend m SharedEventState
modifySharedStateLocked eid f = fst <$> modifySharedStateLockedWith eid (\s -> (f s, ()))

-- | 'modifySharedStateLocked' that also returns an extra value computed from the
-- locked pre-update state — e.g. the act-advance coordinator decides INSIDE the
-- lock whether this call actually consumed the pool (crossed the threshold) and
-- returns that flag, so a concurrent second resolver (which sees the pool already
-- reset) can be told it did NOT consume.
modifySharedStateLockedWith
  :: MonadIO m
  => ArkhamEpicEventId
  -> (SharedEventState -> (SharedEventState, a))
  -> ReaderT SqlBackend m (SharedEventState, a)
modifySharedStateLockedWith eid f = do
  locked <- select do
    e <- from $ table @ArkhamEpicEvent
    where_ $ e.id ==. val eid
    locking forUpdate
    pure e
  case locked of
    [] -> error "modifySharedStateLockedWith: epic event row vanished mid-transaction"
    (Entity _ e : _) -> do
      now <- liftIO getCurrentTime
      let (s1, a) = f (arkhamEpicEventSharedState e)
      P.update
        eid
        [ ArkhamEpicEventSharedState P.=. s1
        , ArkhamEpicEventUpdatedAt P.=. now
        ]
      pure (s1, a)

-- | Revert deltas that were recorded for a particular game step (used by undo).
-- Subtracts each delta's amount from the *current* value under lock; additive
-- deltas commute, so this is correct even if other groups moved the counter in
-- between. The corresponding 'ArkhamEpicStep' rows are deleted.
revertEpicDeltasForGameStep
  :: MonadIO m
  => ArkhamEpicEventId
  -> ArkhamGameId
  -> Int
  -> ReaderT SqlBackend m (Maybe SharedEventState)
revertEpicDeltasForGameStep eid gameId gameStep = do
  stepRows <- select do
    s <- from $ table @ArkhamEpicStep
    where_ $ s.arkhamEpicEventId ==. val eid
    where_ $ s.arkhamGameId ==. just (val gameId)
    where_ $ s.gameStep ==. just (val gameStep)
    pure s
  let deltas = map (arkhamEpicStepDelta . entityVal) stepRows
  if null deltas
    then pure Nothing
    else do
      locked <- select do
        e <- from $ table @ArkhamEpicEvent
        where_ $ e.id ==. val eid
        locking forUpdate
        pure e
      case locked of
        [] -> pure Nothing
        (Entity _ e : _) -> do
          now <- liftIO getCurrentTime
          let s1 = foldl' (flip revertDelta) (arkhamEpicEventSharedState e) deltas
          P.update
            eid
            [ ArkhamEpicEventSharedState P.=. s1
            , ArkhamEpicEventUpdatedAt P.=. now
            ]
          for_ stepRows \(Entity sid _) -> P.delete sid
          pure (Just s1)

{- | The per-game undo FLOOR: the persistence step at/below which undo is walled
off, because crossing it would rewind an epic act-clue advance whose shared
effect (pool reset + generation bump that the OTHER groups then follow) cannot be
locally undone. Set in 'Api.Handler.Arkham.Games.Shared.updateGame' when a group
advances its act IN-GROUP; enforced by 'Api.Handler.Arkham.Undo.stepBack' /
'stepBackToScenarioStep'. 0 (no row) means no floor — ordinary undo.
-}
getGameUndoFloor :: MonadIO m => ArkhamGameId -> ReaderT SqlBackend m Int
getGameUndoFloor gameId = do
  mRow <- P.getBy (UniqueGameUndoFloor gameId)
  pure $ maybe 0 (arkhamGameUndoFloorFloorStep . entityVal) mRow

