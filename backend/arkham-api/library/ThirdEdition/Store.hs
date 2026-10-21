{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Where third-edition tables live. Postgres: a game is state, not cache, and
the Redis cluster they used to live in evicts under memory pressure
('allkeys_lru'), expires a quiet table after a fortnight and loses one outright
if a deploy changes the shape it decodes to.

The Redis store and the bridge that carried its tables across are gone; what
Redis was still holding was brought over at the cutover, and the keys it left
behind expire on their own. Keeping the bridge meant running it at every pod
boot against keys nothing ever deleted, which put a table a host had closed
back in their lobby from a stale snapshot.

Each table keeps a short history of earlier game states for undo.
-}
module ThirdEdition.Store (
  Store (..),
  HistoryChange (..),
  newPostgresStore,
) where

import AH3e.Game (Game)
import Data.Aeson (FromJSON, Value, toJSON)
import Data.Aeson.Types (parseEither, parseJSON)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Traversable (for)
import Data.UUID (UUID)
import Database.Persist qualified as P
import Database.Persist.Sql (
  ConnectionPool,
  PersistValue (..),
  Single (..),
  SqlBackend,
  fromSqlKey,
  rawSql,
  runSqlPool,
  toPersistValue,
 )
import Model
import Relude
import ThirdEdition.Table

-- | What an update does to the undo history.
data HistoryChange = KeepHistory | PushHistory Game | PopHistory

data Store = Store
  { getTable :: UUID -> IO (Maybe (Table, Int))
  -- ^ the table and how many earlier states it can undo to
  , getHistory :: UUID -> IO [Game]
  {- ^ every earlier state the table can undo to, newest first: what an export
  carries so a filed bug can be rewound.
  -}
  , insertTable :: Table -> [Game] -> IO ()
  -- ^ a new table and the history it starts with (newest first), in one write
  , updateTable
      :: UUID
      -> (Table -> Maybe Game -> Either Text (Table, HistoryChange))
      -> IO (Either Text (Table, Int))
  {- ^ atomically: the function sees the table and its most recent earlier
  state, and returns the new table. @Left@ changes nothing.
  -}
  , deleteTable :: UUID -> IO ()
  , listTables :: UserId -> IO ([Table], [Table])
  -- ^ lobbies still open to join, and the tables this user sits at
  }

historyLimit :: Int
historyLimit = 100

-- | How long a table survives with nobody touching it.
staleAfter :: NominalDiffTime
staleAfter = 30 * 24 * 60 * 60

-- ---------------------------------------------------------------- postgres ---

{- | Tables in Postgres, one row each, with the undo history in rows of its own.

An update takes the row's write lock for the length of its transaction rather
than retrying optimistically the way the Redis store had to: two pods answering
the same table queue up instead of racing, and the history rows move with the
table in the same commit.
-}
newPostgresStore :: ConnectionPool -> IO Store
newPostgresStore pool =
  pure
    Store
      { getTable = \tid -> run do
          mrow <- P.get (key tid)
          case mrow of
            Nothing -> pure Nothing
            Just row -> case decodeRow row of
              Nothing -> Nothing <$ warnUnreadable (key tid)
              Just t -> Just . (t,) <$> historyDepth (key tid)
      , getHistory = run . allHistory . key
      , insertTable = \t hist -> run do
          now <- liftIO getCurrentTime
          P.insertKey (key t.id) (rowFor t now)
          seedHistory (key t.id) hist now
          -- A table nobody has touched in a month goes, history and all, as it did
          -- under Redis's two-week expiry. New tables are rare, so this rides along
          -- with one rather than wanting a scheduler of its own.
          P.deleteWhere [ThirdEditionTableUpdatedAt P.<. addUTCTime (negate staleAfter) now]
      , updateTable = \tid f -> run do
          -- the lock is held until this transaction ends, so the read below and
          -- the write that follows it see the same table
          locked <- lockTable tid
          case locked >>= decodeJsonValue of
            Nothing -> pure (Left "No such table")
            Just t -> do
              prev <- latestHistory (key tid)
              case f t prev of
                Left e -> pure (Left e)
                Right (t', change) -> do
                  now <- liftIO getCurrentTime
                  P.repsert (key t'.id) (rowFor t' now)
                  applyHistory (key tid) change
                  Right . (t',) <$> historyDepth (key tid)
      , deleteTable = run . P.delete . key
      , listTables = \uid -> run do
          openRows <- P.selectList [ThirdEditionTableStarted P.==. False] []
          mineRows :: [P.Entity ThirdEditionTable] <-
            rawSql
              "SELECT ?? FROM arkham_third_edition_tables \
              \WHERE payload -> 'seats' @> ?::jsonb"
              [PersistText ("[{\"user\":" <> show (fromSqlKey uid) <> "}]")]
          open <- decodeRows openRows
          mine <- decodeRows mineRows
          pure (filter (not . started) open, mine)
      }
 where
  run :: ReaderT SqlBackend IO a -> IO a
  run act = runSqlPool act pool

  key = ThirdEditionTableKey

  rowFor :: Table -> UTCTime -> ThirdEditionTable
  rowFor t now = ThirdEditionTable t.host (started t) t.version (toJSON t) t.createdAt now

  decodeRow = decodeJsonValue . (.payload)

  decodeRows :: MonadIO m => [P.Entity ThirdEditionTable] -> m [Table]
  decodeRows rows =
    catMaybes <$> for rows \e -> case decodeRow (P.entityVal e) of
      Just t -> pure (Just t)
      Nothing -> Nothing <$ warnUnreadable (P.entityKey e)

  {- A payload this build can no longer parse leaves the table invisible: out of
  the lobby and a 404 when opened, which from the outside is exactly what a table
  somebody closed looks like. Say which one, so the next report of a game that
  vanished has something to go on. A new field on the engine's state must be
  'Maybe', or this is every table at once.
  -}
  warnUnreadable :: MonadIO m => ThirdEditionTableId -> m ()
  warnUnreadable k =
    putStrLn ("third edition: " <> show k <> " does not decode and is invisible in the lobby")

  historyDepth k = P.count [ThirdEditionHistoryTableId P.==. k]

  latestHistory k = listToMaybe <$> historyRows k 1

  allHistory k = historyRows k historyLimit

  historyRows k n = do
    rows <-
      P.selectList [ThirdEditionHistoryTableId P.==. k] [P.Desc ThirdEditionHistoryId, P.LimitTo n]
    pure (mapMaybe (decodeJsonValue . (.game) . P.entityVal) rows)

  -- ids ascend, and the reads above order by them descending, so the oldest
  -- state of a newest-first history has to be inserted first
  seedHistory k hist now =
    P.insertMany_
      [ThirdEditionHistory k (toJSON g) now | g <- reverse (take historyLimit hist)]

  applyHistory k = \case
    KeepHistory -> pure ()
    PushHistory g -> do
      now <- liftIO getCurrentTime
      _ <- P.insert (ThirdEditionHistory k (toJSON g) now)
      -- the oldest states beyond the cap are of no use to anyone and the blobs are not small
      keep <-
        P.selectKeysList
          [ThirdEditionHistoryTableId P.==. k]
          [P.Desc ThirdEditionHistoryId, P.LimitTo historyLimit]
      case keep of
        [] -> pure ()
        _ -> P.deleteWhere [ThirdEditionHistoryTableId P.==. k, ThirdEditionHistoryId P./<-. keep]
    PopHistory -> do
      newest <-
        P.selectKeysList [ThirdEditionHistoryTableId P.==. k] [P.Desc ThirdEditionHistoryId, P.LimitTo 1]
      traverse_ P.delete newest

  -- SELECT ... FOR UPDATE: persistent has no combinator for it, and the lock is
  -- the point of the query
  lockTable :: UUID -> ReaderT SqlBackend IO (Maybe Value)
  lockTable tid = do
    rows <-
      rawSql
        "SELECT payload FROM arkham_third_edition_tables WHERE id = ? FOR UPDATE"
        [toPersistValue tid]
    pure (unSingle <$> listToMaybe rows)

decodeJsonValue :: FromJSON a => Value -> Maybe a
decodeJsonValue = rightToMaybe . parseEither parseJSON
