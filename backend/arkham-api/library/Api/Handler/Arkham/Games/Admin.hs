{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Api.Handler.Arkham.Games.Admin (
  getApiV1AdminR,
  getApiV1AdminStatsR,
  getApiV1AdminRoomsR,
  getApiV1AdminGameR,
  getApiV1AdminFindGameR,
  putApiV1AdminGameR,
  getApiV1AdminGamesR,
  getApiV1AdminActiveGamesR,
  putApiV1AdminGameRawR,
  getApiV1AdminRoomR,
  deleteApiV1AdminRoomR,
  postApiV1AdminRoomProbeR,
  postApiV1AdminRoomSweepR,
) where

import Api.Arkham.Epic (lookupGameEvent)
import Api.Arkham.Helpers
import Api.Arkham.RoomOps
import Api.Handler.Arkham.Games.Shared
import Arkham.Game
import Conduit
import Control.Concurrent.MVar
import Data.Map.Strict qualified as Map
import Data.Time.Clock
import Database.Esqueleto.Experimental hiding (update, (=.))
import Entity.Answer
import Entity.Arkham.GameRaw
import Entity.Arkham.Player
import Import hiding (delete, exists, on, readMVar, (==.), (>=.))

data AdminData = AdminData
  { currentUsers :: Int
  , activeUsers :: Int
  , roomData :: [RoomData]
  , activeGames :: [GameDetailsEntry]
  , recentGames :: [GameDetailsEntry]
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data AdminStats = AdminStats
  { currentUsers :: Int
  , activeUsers :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data RoomData = RoomData
  { roomClients :: Int
  , roomWatchers :: Int
  {- ^ How many of those clients are spectators. The pair is what makes the
  list readable: a room whose clients are all watchers has nobody playing it,
  which is the one thing the old count-only row could not say.
  -}
  , roomLastUpdatedAt :: Maybe UTCTime
  , roomArkhamGameId :: ArkhamGameId
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

selectCount :: SqlQuery a -> DB Int
selectCount inner = fmap (sum . map unValue . toList) . selectOne $ inner $> countRows

getApiV1AdminR :: Handler AdminData
getApiV1AdminR = do
  AdminStats {..} <- getApiV1AdminStatsR
  roomData <- getRoomData
  activeGames <- getActiveGames roomData
  recentGames <- runDB $ getRecentGames 20
  pure $ AdminData {..}

getApiV1AdminStatsR :: Handler AdminStats
getApiV1AdminStatsR = do
  recent <- addUTCTime (negate (14 * nominalDay)) <$> liftIO getCurrentTime

  runDB do
    currentUsers <- selectCount $ from $ table @User
    activeUsers <-
      fmap (sum . map unValue . toList) . selectOne $ do
        (users :& _players :& games) <- from do
          table @User
            `innerJoin` table @ArkhamPlayer
              `on` (\(users :& players) -> users.id ==. players.userId)
            `innerJoin` table @ArkhamGame
              `on` (\(_ :& players :& games) -> players.arkhamGameId ==. games.id)
        where_ (games.updatedAt >=. val recent)
        pure (countDistinct users.id)

    pure $ AdminStats {..}

getActiveGames :: [RoomData] -> Handler [GameDetailsEntry]
getActiveGames roomData = runDB do
  games <- select do
    games <- from $ table @ArkhamGameRaw
    where_ (games.id `in_` valList (coerce $ map (.roomArkhamGameId) roomData))
    pure games
  traverse (`tryGameDetailsEntry` 0) games

getApiV1AdminGameR :: ArkhamGameId -> Handler GetGameJson
getApiV1AdminGameR gameId = do
  servePongAware $ gameStream gameId
  g <- runDB $ get404 gameId
  let Game {..} = g.currentData
  gameLog <- runDB $ getGameLogTail gameId gameLogTailSize
  let player = gameActivePlayerId
  mEvt <- runDB $ lookupGameEvent gameId
  pure
    $ GetGameJson
      (Just player)
      g.variant
      (PublicGame gameId g.name gameLog.entries g.currentData)
      (entityKey . fst <$> mEvt)

getApiV1AdminFindGameR :: ArkhamPlayerId -> Handler GameDetailsEntry
getApiV1AdminFindGameR playerId = do
  runDB do
    player <- get404 playerId
    g <- getEntity404 $ coerce player.arkhamGameId
    tryGameDetailsEntry g 0

getRecentGames :: Int64 -> DB [GameDetailsEntry]
getRecentGames n = do
  games <- select do
    games <- from $ table @ArkhamGameRaw
    orderBy [desc games.updatedAt]
    limit n
    pure games
  traverse (`tryGameDetailsEntry` 0) games

getApiV1AdminGamesR :: Handler [GameDetailsEntry]
getApiV1AdminGamesR = runDB $ getRecentGames 20

getApiV1AdminActiveGamesR :: Handler [GameDetailsEntry]
getApiV1AdminActiveGamesR = getRoomData >>= getActiveGames

putApiV1AdminGameR :: ArkhamGameId -> Handler ()
putApiV1AdminGameR gameId = do
  response <- requireCheckJsonBody
  mRoom <- lookupRoom gameId
  updateGame mempty response gameId mRoom

-- TODO: Make this a websocket message
putApiV1AdminGameRawR :: ArkhamGameId -> Handler ()
putApiV1AdminGameRawR gameId = do
  response <- requireCheckJsonBody @_ @RawGameJsonPut
  mRoom <- lookupRoom gameId
  updateGame mempty (Raw response.gameMessage) gameId mRoom

getApiV1AdminRoomsR :: Handler [RoomData]
getApiV1AdminRoomsR = getRoomData

{- | @arkham_games.updated_at@ for a set of games, as ONE projection.

Deliberately not a per-room @get@. That is a row fetch, and the row carries
@current_data@ -- the entire game state, which is 57-206 KB of JSON on a real
mid-campaign game -- so reading one timestamp beside it meant pulling and
decoding every live game on the site, once per open room, every time an admin
opened the dashboard.
-}
gameUpdatedAts :: [ArkhamGameId] -> DB (Map ArkhamGameId UTCTime)
gameUpdatedAts [] = pure mempty
gameUpdatedAts gameIds = do
  rows <- select do
    games <- from $ table @ArkhamGame
    where_ (games.id `in_` valList gameIds)
    pure (games.id, games.updatedAt)
  pure $ Map.fromList [(gameId, updatedAt) | (Value gameId, Value updatedAt) <- rows]

getRoomData :: Handler [RoomData]
getRoomData = do
  -- When a Redis broker is configured, we aggregate room/client counts
  -- across every API server through `arkham:rooms`. Otherwise fall back
  -- to this server's in-memory map (single-server / dev setups).
  mRedisCounts <- getRedisRoomCounts
  tallies <- case mRedisCounts of
    Just counts -> do
      watchers <- getRedisWatcherCounts
      pure
        [ (gameId, clients, Map.findWithDefault 0 gameId watchers)
        | (gameId, clients) <- Map.assocs counts
        ]
    Nothing -> do
      roomsVar <- getsApp appGameRooms
      rooms <- liftIO $ readMVar roomsVar
      liftIO $ traverse countsFor (Map.assocs rooms)
  -- A room with no row here outlived the game behind it, which the client
  -- renders as "deleted" rather than as a missing timestamp.
  updatedAts <- runDB $ gameUpdatedAts [gameId | (gameId, _, _) <- tallies]
  pure
    [ RoomData
        { roomClients = clients
        , roomWatchers = watchers
        , roomLastUpdatedAt = Map.lookup gameId updatedAts
        , roomArkhamGameId = gameId
        }
    | (gameId, clients, watchers) <- tallies
    ]
 where
  countsFor (gameId, room) =
    (,,) gameId <$> roomClientCount room <*> roomWatcherCount room

{- | One seat at the game, and whether anybody is sitting in it right now.

The rooms page's own counts cannot answer "is this game being played?" -- a
room with three clients and an empty table looks the same as a room with three
clients and three players. The seats come from @arkham_players@, which is the
only record of who the game belongs to, and the sockets say which of them is
present.
-}
data RoomSeat = RoomSeat
  { userId :: Text
  , username :: Text
  , investigatorId :: Text
  , connected :: Bool
  -- ^ Holding a player socket (not a spectator one) on some pod.
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

{- | One websocket on the room, with the account behind it named.

'Api.Arkham.RoomOps.RoomConnection' carries a user id because it crosses
Redis and cannot run a query; this is that, resolved.
-}
data RoomConnectionView = RoomConnectionView
  { pod :: Text
  , connectionId :: Int
  , userId :: Maybe Text
  , username :: Maybe Text
  , spectator :: Bool
  , connectedAt :: UTCTime
  , connectedForSeconds :: Int
  , lastPongAt :: Maybe UTCTime
  , lastPongSecondsAgo :: Maybe Int
  {- ^ How long ago this socket last proved it was there. A number much larger
  than the keepalive interval is a socket whose client has gone without the
  server noticing.
  -}
  , queued :: Int
  , overflowed :: Bool
  , closing :: Bool
  , probeAnswered :: Maybe Bool
  -- ^ Only set by a probe or a sweep; 'Nothing' means it was never asked.
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

{- | Everything an admin needs to decide what is wrong with one room.

Three sources, deliberately kept apart rather than reconciled into one number:
the game row, the Redis tally, and the sockets the pods actually hold. A room
that will not close is a room whose tally and whose sockets disagree, so a view
that averaged them would hide the only thing worth seeing.
-}
data RoomDetails = RoomDetails
  { gameId :: ArkhamGameId
  , gameName :: Maybe Text
  -- ^ 'Nothing' when the game row is gone and the room outlived it.
  , gameUpdatedAt :: Maybe UTCTime
  , tally :: RoomTally
  , podsAnswered :: [Text]
  , podsHoldingRoom :: [Text]
  , connections :: [RoomConnectionView]
  , playerConnections :: Int
  , spectatorConnections :: Int
  , seats :: [RoomSeat]
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

{- | What a sweep did, or why it would not.

Carries the probe it decided on, so a refusal can be checked rather than taken
on trust: the connection that answered is named, with the account behind it.
-}
data RoomSweepResult = RoomSweepResult
  { swept :: Bool
  , detail :: Text
  , livePlayers :: [RoomConnectionView]
  , room :: RoomDetails
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

{- | Collect one op's replies and present them as a room.

The connections come back from the pods carrying user ids and nothing else --
they crossed Redis, where there is no database to ask -- so the names are put
on here, for the seats and for anyone on a socket who does not hold one.
-}
roomDetailsFrom :: ArkhamGameId -> [RoomOpReply] -> Handler RoomDetails
roomDetailsFrom gameId replies = do
  now <- liftIO getCurrentTime
  tally <- getRoomTally gameId
  let conns = concatMap (.roomReplyConnections) replies
      connUserIds = mapMaybe (\c -> fromPathPiece =<< c.connUserId) conns
  -- Two columns, not the row: 'arkham_games.current_data' is the whole game
  -- blob and this needs the game's name.
  (mGame, seatRows, socketUsers) <- runDB do
    mGame <- selectOne do
      games <- from $ table @ArkhamGame
      where_ (games.id ==. val gameId)
      pure (games.name, games.updatedAt)
    seatRows <- select do
      (players :& users) <- from do
        table @ArkhamPlayer
          `innerJoin` table @User
            `on` (\(players :& users) -> players.userId ==. users.id)
      where_ (players.arkhamGameId ==. val gameId)
      pure (players, users)
    socketUsers <-
      if null connUserIds
        then pure []
        else select do
          users <- from $ table @User
          where_ (users.id `in_` valList connUserIds)
          pure users
    pure (mGame, seatRows, socketUsers)
  let nameOf =
        Map.fromList
          $ [(toPathPiece (entityKey u), (entityVal u).username) | u <- socketUsers]
          <> [(toPathPiece (entityKey u), (entityVal u).username) | (_, u) <- seatRows]
      -- A seat is "connected" when it holds a PLAYER socket. A player who also
      -- has the game open in a second tab as a watcher is still seated; a
      -- spectator who happens to own a seat in some other game is not.
      seatedUserIds = [uid | c <- conns, not c.connSpectator, Just uid <- [c.connUserId]]
      asView c =
        RoomConnectionView
          { pod = c.connPod
          , connectionId = c.connId
          , userId = c.connUserId
          , username = flip Map.lookup nameOf =<< c.connUserId
          , spectator = c.connSpectator
          , connectedAt = c.connConnectedAt
          , connectedForSeconds = round (diffUTCTime now c.connConnectedAt)
          , lastPongAt = c.connLastPongAt
          , lastPongSecondsAgo = round . diffUTCTime now <$> c.connLastPongAt
          , queued = c.connQueued
          , overflowed = c.connOverflowed
          , closing = c.connClosing
          , probeAnswered = c.connProbeAnswered
          }
  pure
    $ RoomDetails
      { gameId
      , gameName = unValue . fst <$> mGame
      , gameUpdatedAt = unValue . snd <$> mGame
      , tally
      , podsAnswered = map (.roomReplyPod) replies
      , podsHoldingRoom = [r.roomReplyPod | r <- replies, r.roomReplyHadRoom]
      , connections = map asView conns
      , playerConnections = length [() | c <- conns, not c.connSpectator]
      , spectatorConnections = length [() | c <- conns, c.connSpectator]
      , seats =
          [ RoomSeat
              { userId = toPathPiece (entityKey u)
              , username = (entityVal u).username
              , investigatorId = (entityVal p).investigatorId
              , connected = toPathPiece (entityKey u) `elem` seatedUserIds
              }
          | (p, u) <- seatRows
          ]
      }

-- GET /api/v1/admin/rooms/#ArkhamGameId
--
-- Look one room up by game id, whether or not it is on the pod answering this
-- request and whether or not it appears in the rooms list at all -- the list is
-- built from the Redis tally, and a room missing from the tally while a pod
-- still holds it is precisely a case worth being able to ask about.
getApiV1AdminRoomR :: ArkhamGameId -> Handler RoomDetails
getApiV1AdminRoomR gameId = runRoomOp gameId InspectRoom >>= roomDetailsFrom gameId

-- POST /api/v1/admin/rooms/#ArkhamGameId/probe
--
-- Ping every socket on the room and report which answered. Changes nothing;
-- a socket that does not answer is still attached when this returns.
postApiV1AdminRoomProbeR :: ArkhamGameId -> Handler RoomDetails
postApiV1AdminRoomProbeR gameId = runRoomOp gameId ProbeRoom >>= roomDetailsFrom gameId

{- | POST \/api\/v1\/admin\/rooms\/#ArkhamGameId\/sweep

Close a room that nobody is playing any more.

Probe first, then decide: a player socket that answers the ping is somebody at
the table, and the sweep refuses rather than throwing them out. Spectators are
no reason to keep a room: once the seats are gone nothing further will happen
on that board, which is the same judgement 'sweepAbandonedRooms' makes on its
own timer.

A player socket that does NOT answer is what this exists for. It still holds a
seat as far as the room and the Redis tally are concerned, and it will go on
holding it forever, because nothing in the ordinary path can tell a silent
socket from a quiet one.

A refusal still reconciles the tally. The probe has just counted every socket
the fleet is holding, and the tally is only ever moved a step at a time by a
join or a leave, so a pod that died with sockets attached leaves its share in
there forever -- which reads as seated players, which is what stops the room
ever being swept. Leaving a live room open is the right call; leaving the drift
behind with it is not, and the two are independent.

@?force=true@ skips the probe's verdict and closes regardless, for the case
where an admin can see from the detail view that the sockets are dead and does
not want to argue with a probe about it.
-}
postApiV1AdminRoomSweepR :: ArkhamGameId -> Handler RoomSweepResult
postApiV1AdminRoomSweepR gameId = do
  forced <- (== Just "true") <$> lookupGetParam "force"
  probe <- runRoomOp gameId ProbeRoom
  probed <- roomDetailsFrom gameId probe
  let live = [c | c <- probed.connections, not c.spectator, c.probeAnswered == Just True]
  if not forced && not (null live)
    then do
      let held = length probed.connections
      let drift = maybe 0 (subtract held) probed.tally.tallyClients
      setRoomTallies gameId held probed.spectatorConnections
      reconciled <- roomDetailsFrom gameId probe
      pure
        $ RoomSweepResult
          { swept = False
          , detail =
              tshow (length live)
                <> " player connection(s) answered the probe, so the room was left open"
                <> if drift > 0
                  then
                    "; cleared "
                      <> tshow drift
                      <> " socket(s) Redis was counting that no pod holds"
                  else "; Redis tally already matched the sockets held"
          , livePlayers = live
          , room = reconciled
          }
    else do
      closed <- runRoomOp gameId (CloseRoom "swept")
      -- The sockets are gone; the tally they were counted in has to go with
      -- them or the room is still on the list a moment from now, which is the
      -- failure that made this endpoint necessary in the first place.
      purgeRoomTallies gameId
      after <- roomDetailsFrom gameId closed
      pure
        $ RoomSweepResult
          { swept = any (.roomReplyClosed) closed
          , detail =
              if any (.roomReplyClosed) closed
                then
                  "Closed on "
                    <> tshow (length [() | r <- closed, r.roomReplyClosed])
                    <> " pod(s); Redis tallies cleared"
                else "No pod was holding a room for this game; Redis tallies cleared"
          , livePlayers = live
          , room = after
          }

{- | DELETE \/api\/v1\/admin\/rooms\/#ArkhamGameId

Drop a room and evict everyone on it, no questions asked.

Goes through 'runRoomOp' rather than 'deleteRoom'. It used to be the local
call, which meant it dropped the room only if the pod answering the request
happened to be the pod holding it -- a one-in-six chance on the live fleet, and
the other five times it returned 204 having done nothing at all while the room
stayed on the list. See "Api.Arkham.RoomOps".
-}
deleteApiV1AdminRoomR :: ArkhamGameId -> Handler RoomDetails
deleteApiV1AdminRoomR gameId = do
  replies <- runRoomOp gameId (CloseRoom "deleted")
  purgeRoomTallies gameId
  roomDetailsFrom gameId replies
