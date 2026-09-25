{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Third edition tables: a lobby of seats that turns into a game when its host
starts it. Every change is published to the table's room, so each seated
player (and anyone watching) sees it; the websocket itself is a read-only feed
and moves arrive over REST.

Everything third-edition specific in the API lives here and under
"ThirdEdition", reached from @/api/v1/3ed@.
-}
module Api.Handler.ThirdEdition (
  getApiV1ThirdEditionCatalogR,
  getApiV1ThirdEditionTablesR,
  postApiV1ThirdEditionTablesR,
  getApiV1ThirdEditionTableR,
  deleteApiV1ThirdEditionTableR,
  postApiV1ThirdEditionJoinR,
  postApiV1ThirdEditionLeaveR,
  postApiV1ThirdEditionStartR,
  postApiV1ThirdEditionAnswerR,
  postApiV1ThirdEditionDebugR,
  postApiV1ThirdEditionUndoR,
) where

import AH3e.Engine (GameOptions (..), answer, applyDebug, newGame, runEngine)
import AH3e.Game (Game)
import AH3e.Types.Card (Expansion (CoreSet))
import AH3e.Types.Ids (PlayerId (..))
import AH3e.Types.State (DebugAction, GameMode)
import AH3e.View (catalogView, gameView)
import Api.Arkham.Helpers (joinRoomIn, releaseRoomIfEmpty)
import Api.Handler.Arkham.Games.Shared (publishOrWarn, streamRoom, websocketConnectionOptions)
import Control.Monad.Random (getRandomR)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Database.Redis (RedisChannel)
import Import
import Network.HTTP.Types (Status, status400, status403, status404)
import ThirdEdition.Store (HistoryChange (..), Store (..))
import ThirdEdition.Table
import Yesod.WebSockets (webSocketsOptions)

tableChannel :: UUID -> RedisChannel
tableChannel tid = "ah3e-" <> UUID.toASCIIBytes tid

getStore :: Handler Store
getStore = getsApp appThirdEditionStore

failWith :: Status -> Text -> Handler a
failWith status msg = sendResponseStatus status (object ["error" .= msg])

getApiV1ThirdEditionCatalogR :: Handler Value
getApiV1ThirdEditionCatalogR = pure (object catalogView)

getApiV1ThirdEditionTablesR :: Handler Value
getApiV1ThirdEditionTablesR = do
  userId <- getRequestUserId
  store <- getStore
  (open, mine) <- liftIO (store.listTables userId)
  let newest = sortOn (Down . (.createdAt))
  pure $ object ["open" .= map tableSummary (newest open), "mine" .= map tableSummary (newest mine)]

data NewTable = NewTable
  { name :: Maybe Text
  , seats :: Int
  , expansions :: [Expansion]
  , mode :: GameMode
  , debug :: Bool
  }
  deriving stock Generic
  deriving anyclass FromJSON

postApiV1ThirdEditionTablesR :: Handler Value
postApiV1ThirdEditionTablesR = do
  Entity userId user <- getRequestUser
  body <- (requireCheckJsonBody :: Handler NewTable)
  unless (body.seats >= 1 && body.seats <= 6) $ failWith status400 "A table seats one to six players"
  tid <- liftIO nextRandom
  now <- liftIO getCurrentTime
  let table =
        Table
          { id = tid
          , name = fromMaybe (userUsername user <> "'s table") (mfilter (/= "") body.name)
          , host = userId
          , hostName = userUsername user
          , seats =
              [ Seat
                  n
                  (if n == 1 then Just userId else Nothing)
                  (if n == 1 then Just (userUsername user) else Nothing)
              | n <- [1 .. body.seats]
              ]
          , -- the core set is always in play
            options = TableOptions (ordNub (CoreSet : body.expansions)) body.mode body.debug
          , game = Nothing
          , version = 0
          , createdAt = now
          }
  store <- getStore
  liftIO (store.insertTable table)
  pure (tableView Nothing table 0)

{- | The table as it stands, or its live feed when the request is a websocket
upgrade. A client should open the feed first and then fetch, keeping whichever
copy has the higher @version@.
-}
getApiV1ThirdEditionTableR :: UUID -> Handler Value
getApiV1ThirdEditionTableR tid = do
  _ <- getRequestUserId
  wsOptions <- websocketConnectionOptions
  webSocketsOptions wsOptions
    $ streamRoom
      (joinRoomIn appThirdEditionRooms tableChannel tid)
      (void $ releaseRoomIfEmpty appThirdEditionRooms tid)
  store <- getStore
  liftIO (store.getTable tid) >>= \case
    Nothing -> failWith status404 "No such table"
    Just (t, depth) -> pure (tableView Nothing t depth)

deleteApiV1ThirdEditionTableR :: UUID -> Handler ()
deleteApiV1ThirdEditionTableR tid = do
  userId <- getRequestUserId
  store <- getStore
  liftIO (store.getTable tid) >>= \case
    Nothing -> failWith status404 "No such table"
    Just (t, _) -> do
      unless (t.host == userId) $ failWith status403 "Only the host can close the table"
      liftIO (store.deleteTable tid)
      publishTable tid (object ["tag" .= ("TableClosed" :: Text)])

newtype SeatRequest = SeatRequest {seat :: Maybe Int}
  deriving stock Generic
  deriving anyclass FromJSON

postApiV1ThirdEditionJoinR :: UUID -> Handler Value
postApiV1ThirdEditionJoinR tid = do
  Entity userId user <- getRequestUser
  req <- (requireCheckJsonBody :: Handler SeatRequest)
  mutate "join" tid \t _ -> do
    when (started t) $ Left "The game has already started"
    (,KeepHistory) <$> takeSeat userId (userUsername user) req.seat t

postApiV1ThirdEditionLeaveR :: UUID -> Handler Value
postApiV1ThirdEditionLeaveR tid = do
  userId <- getRequestUserId
  req <- (requireCheckJsonBody :: Handler SeatRequest)
  mutate "leave" tid \t _ -> do
    when (started t) $ Left "The game has already started"
    pure (leaveSeat userId req.seat t, KeepHistory)

postApiV1ThirdEditionStartR :: UUID -> Handler Value
postApiV1ThirdEditionStartR tid = do
  userId <- getRequestUserId
  seed <- liftIO (getRandomR (0, 2 ^ (30 :: Int)))
  mutate "start" tid \t _ -> do
    unless (t.host == userId) $ Left "Only the host can start the game"
    when (started t) $ Left "The game has already started"
    unless (null (freeSeats t)) $ Left "Every seat needs a player first"
    let opts = GameOptions {expansions = t.options.expansions, mode = t.options.mode, debug = t.options.debug}
    g <- newGame [PlayerId s.player | s <- t.seats] seed opts
    g' <- first tshow (runEngine g)
    pure (withGame g' t, KeepHistory)

{- | @version@ is the table the player was looking at. Choices are indexes, so
an answer to a question that has since moved on is refused, not misapplied.
-}
data AnswerRequest = AnswerRequest {player :: Int, choice :: Int, version :: Maybe Int}
  deriving stock Generic
  deriving anyclass FromJSON

-- | Only the player holding a seat answers for it.
postApiV1ThirdEditionAnswerR :: UUID -> Handler Value
postApiV1ThirdEditionAnswerR tid = do
  userId <- getRequestUserId
  req <- (requireCheckJsonBody :: Handler AnswerRequest)
  mutate "answer" tid \t _ -> do
    unless (holdsSeat userId req.player t) $ Left "That isn't your seat"
    unless (maybe True (== t.version) req.version)
      $ Left "The table changed before your answer arrived; choose again"
    step t (answer (PlayerId req.player) req.choice)

-- | Debug actions (tables created with debug on) and undo are open to anyone seated.
postApiV1ThirdEditionDebugR :: UUID -> Handler Value
postApiV1ThirdEditionDebugR tid = do
  userId <- getRequestUserId
  action <- (requireCheckJsonBody :: Handler DebugAction)
  mutate "debug" tid \t _ -> do
    unless (isSeated userId t) $ Left "Only seated players can use debug actions"
    step t (applyDebug action)

postApiV1ThirdEditionUndoR :: UUID -> Handler Value
postApiV1ThirdEditionUndoR tid = do
  userId <- getRequestUserId
  mutate "undo" tid \t prev -> do
    unless (isSeated userId t) $ Left "Only seated players can undo"
    earlier <- maybe (Left "Nothing to undo") Right prev
    pure (withGame earlier t, PopHistory)

-- | Run the engine on the table's game, keeping the state it replaces for undo.
step :: Show e => Table -> (Game -> Either e Game) -> Either Text (Table, HistoryChange)
step t f = do
  g <- maybe (Left "The game hasn't started") Right t.game
  g' <- first tshow (f g)
  pure (withGame g' t, PushHistory g)

{- | Apply a change atomically, then send the new table to everyone in its room.
The cause lets a client tell an undo (cards going back) from a move.
-}
mutate
  :: Text -> UUID -> (Table -> Maybe Game -> Either Text (Table, HistoryChange)) -> Handler Value
mutate cause tid f = do
  store <- getStore
  liftIO (store.updateTable tid \t prev -> first bumpVersion <$> f t prev) >>= \case
    Left e -> failWith status400 e
    Right (t, depth) -> do
      let v = tableView (Just cause) t depth
      publishTable tid (object ["tag" .= ("TableUpdate" :: Text), "contents" .= v])
      pure v

publishTable :: UUID -> Value -> Handler ()
publishTable tid v =
  getsApp appMessageBroker >>= \case
    RedisBroker conn _ -> publishOrWarn conn (tableChannel tid) v
    WebSocketBroker -> do
      rooms <- getsApp appThirdEditionRooms
      room <- Map.lookup tid <$> readMVar rooms
      for_ room \r -> broadcastToRoom r (Aeson.encode v)

seatsJson :: Table -> Value
seatsJson t = Aeson.toJSON [object ["player" .= s.player, "username" .= s.username] | s <- t.seats]

tableSummary :: Table -> Value
tableSummary t =
  object
    [ "id" .= t.id
    , "name" .= t.name
    , "hostName" .= t.hostName
    , "seats" .= seatsJson t
    , "options" .= t.options
    , "started" .= started t
    , "createdAt" .= t.createdAt
    ]

tableView :: Maybe Text -> Table -> Int -> Value
tableView cause t depth =
  object
    [ "id" .= t.id
    , "cause" .= cause
    , "name" .= t.name
    , "hostName" .= t.hostName
    , "seats" .= seatsJson t
    , "options" .= t.options
    , "started" .= started t
    , "version" .= t.version
    , "canUndo" .= (depth > 0)
    , "view" .= fmap gameView t.game
    ]
