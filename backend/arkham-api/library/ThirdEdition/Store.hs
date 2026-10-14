{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Where third-edition tables live. Postgres, as of the store below: a game is
state, not cache, and the Redis cluster it used to live in evicts under memory
pressure ('allkeys_lru'), expires a quiet table after 'tableTtl' and loses one
outright if a deploy changes the shape it decodes to.

The Redis store is kept only for the bridge in 'importRedisTables'.

Each table keeps a short history of earlier game states for undo.
-}
module ThirdEdition.Store (
  Store (..),
  HistoryChange (..),
  newRedisStore,
  newPostgresStore,
  importRedisTables,
) where

import AH3e.Game (Game)
import Control.Exception (throwIO)
import Data.Aeson (FromJSON, Value, decodeStrict, encode, toJSON)
import Data.Aeson.Types (parseEither, parseJSON)
import Data.ByteString qualified as BS
import Data.List ((\\))
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
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
import Database.Redis (Connection, Queued, RedisTx, TxResult (..))
import Database.Redis qualified as R
import Model
import Relude
import System.IO.Error (userError)
import ThirdEdition.Table

-- | What an update does to the undo history.
data HistoryChange = KeepHistory | PushHistory Game | PopHistory

data Store = Store
  { getTable :: UUID -> IO (Maybe (Table, Int))
  -- ^ the table and how many earlier states it can undo to
  , insertTable :: Table -> IO ()
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

-- | An idle table is dropped after two weeks.
tableTtl :: Integer
tableTtl = 60 * 60 * 24 * 14

tableKey, historyKey :: UUID -> ByteString
tableKey tid = "ah3e:table:" <> UUID.toASCIIBytes tid
historyKey tid = "ah3e:history:" <> UUID.toASCIIBytes tid

-- | Ids of tables that haven't started yet.
openKey :: ByteString
openKey = "ah3e:open"

-- | Ids of the tables a user sits at.
userKey :: UserId -> ByteString
userKey uid = "ah3e:user:" <> encodeUtf8 (show @Text (fromSqlKey uid))

newRedisStore :: Connection -> IO Store
newRedisStore conn =
  pure
    Store
      { getTable = \tid -> redis do
          t <- ok (R.get (tableKey tid))
          n <- ok (R.llen (historyKey tid))
          pure $ (,fromInteger n) <$> (t >>= decodeJson)
      , insertTable = \t -> void $ redis $ R.multiExec $ writeTable Nothing t []
      , updateTable = \tid f -> retrying (10 :: Int) (redis (attempt tid f))
      , deleteTable = \tid -> redis do
          mt <- (>>= decodeJson) <$> ok (R.get (tableKey tid))
          void $ R.multiExec do
            _ <- R.del (tableKey tid :| [historyKey tid])
            _ <- R.srem openKey (pure (UUID.toASCIIBytes tid))
            for_ (maybe [] seatedUsers mt) \u -> R.srem (userKey u) (pure (UUID.toASCIIBytes tid))
            pure (pure ())
      , listTables = \uid -> redis do
          open <- ok (R.smembers openKey)
          mine <- ok (R.smembers (userKey uid))
          openTables <- fetch openKey open
          myTables <- fetch (userKey uid) mine
          pure (filter (not . started) openTables, myTables)
      }
 where
  redis :: R.Redis a -> IO a
  redis = R.runRedis conn

  ok :: R.Redis (Either R.Reply a) -> R.Redis a
  ok act = act >>= either (\e -> liftIO (throwIO (userError ("redis: " <> show e)))) pure

  -- expired tables leave their ids behind in the index sets; prune those as we go
  fetch :: ByteString -> [ByteString] -> R.Redis [Table]
  fetch set ids = case nonEmpty ids of
    Nothing -> pure []
    Just keys -> do
      found <- ok (R.mget (fmap ("ah3e:table:" <>) keys))
      let stale = [i | (i, Nothing) <- zip ids found]
      whenJust (nonEmpty stale) (void . ok . R.srem set)
      pure (mapMaybe (>>= decodeJson) found)

  retrying :: Int -> IO (Maybe (Either Text a)) -> IO (Either Text a)
  retrying 0 _ = pure (Left "The table is busy, try again")
  retrying n act = act >>= maybe (retrying (n - 1) act) pure

  -- one optimistic round: WATCH, read, decide, then MULTI/EXEC. EXEC aborts
  -- if another pod wrote the table meanwhile, and we go around again.
  attempt
    :: UUID
    -> (Table -> Maybe Game -> Either Text (Table, HistoryChange))
    -> R.Redis (Maybe (Either Text (Table, Int)))
  attempt tid f = do
    _ <- ok (R.watch [tableKey tid, historyKey tid])
    mt <- (>>= decodeJson) <$> ok (R.get (tableKey tid))
    prev <- (>>= decodeJson) <$> ok (R.lindex (historyKey tid) 0)
    depth <- fromInteger <$> ok (R.llen (historyKey tid))
    case mt of
      Nothing -> Just (Left "No such table") <$ R.unwatch
      Just t -> case f t prev of
        Left e -> Just (Left e) <$ R.unwatch
        Right (t', change) -> do
          result <- R.multiExec (writeTable (Just t) t' (historyOps change))
          let depth' = case change of
                KeepHistory -> depth
                PushHistory _ -> min historyLimit (depth + 1)
                PopHistory -> max 0 (depth - 1)
          pure case result of
            TxSuccess _ -> Just (Right (t', depth'))
            TxAborted -> Nothing
            TxError e -> Just (Left ("redis: " <> toText e))
   where
    historyOps = \case
      KeepHistory -> []
      PushHistory g ->
        [ void <$> R.lpush (historyKey tid) (pure (toStrict (encode g)))
        , void <$> R.ltrim (historyKey tid) 0 (fromIntegral historyLimit - 1)
        ]
      PopHistory -> [void <$> R.lpop (historyKey tid)]

  -- the table, its history's expiry, and the lobby and per-user indexes
  writeTable :: Maybe Table -> Table -> [RedisTx (Queued ())] -> RedisTx (Queued ())
  writeTable old t extra = do
    let tid = UUID.toASCIIBytes t.id
        leftUsers = maybe [] seatedUsers old \\ seatedUsers t
    _ <- R.setex (tableKey t.id) tableTtl (toStrict (encode t))
    sequence_ extra
    _ <- R.expire (historyKey t.id) tableTtl
    _ <-
      if started t
        then void <$> R.srem openKey (pure tid)
        else void <$> R.sadd openKey (pure tid)
    for_ (seatedUsers t) \u -> do
      _ <- R.sadd (userKey u) (pure tid)
      R.expire (userKey u) tableTtl
    for_ leftUsers \u -> R.srem (userKey u) (pure tid)
    pure (pure ())

decodeJson :: FromJSON a => ByteString -> Maybe a
decodeJson = decodeStrict

-- ---------------------------------------------------------------- postgres ---

{- | Tables in Postgres, one row each, with the undo history in rows of its own.

An update takes the row's write lock for the length of its transaction rather
than retrying optimistically the way the Redis store has to: two pods answering
the same table queue up instead of racing, and the history rows move with the
table in the same commit.
-}
newPostgresStore :: ConnectionPool -> IO Store
newPostgresStore pool =
  pure
    Store
      { getTable = \tid -> run do
          mrow <- P.get (key tid)
          case decodeRow =<< mrow of
            Nothing -> pure Nothing
            Just t -> Just . (t,) <$> historyDepth (key tid)
      , insertTable = \t -> run do
          now <- liftIO getCurrentTime
          P.insertKey (key t.id) (rowFor t now)
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
          let decode' :: [P.Entity ThirdEditionTable] -> [Table]
              decode' = mapMaybe (decodeRow . P.entityVal)
          pure (filter (not . started) (decode' openRows), decode' mineRows)
      }
 where
  run :: ReaderT SqlBackend IO a -> IO a
  run act = runSqlPool act pool

  key = ThirdEditionTableKey

  rowFor :: Table -> UTCTime -> ThirdEditionTable
  rowFor t now = ThirdEditionTable t.host (started t) t.version (toJSON t) t.createdAt now

  decodeRow = decodeJsonValue . (.payload)

  historyDepth k = P.count [ThirdEditionHistoryTableId P.==. k]

  latestHistory k = do
    rows <-
      P.selectList [ThirdEditionHistoryTableId P.==. k] [P.Desc ThirdEditionHistoryId, P.LimitTo 1]
    pure (decodeJsonValue . (.game) . P.entityVal =<< listToMaybe rows)

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

{- | Move whatever tables are still in Redis into the store, once, at startup.

ponytail: a bridge, not a feature. Every table Redis is holding when the
Postgres store ships would otherwise be unreachable; the undo history is left
behind, since a game matters and a stack of its earlier states does not. Delete
this (and the Redis store with it) a deploy or two later.
-}
importRedisTables :: Connection -> Store -> IO Int
importRedisTables conn store = do
  tids <- R.runRedis conn (scanTables R.cursor0 [])
  length <$> filterM bring tids
 where
  scanTables cursor acc = do
    -- the third argument is SCAN's TYPE filter, which we have no use for
    res <-
      R.scanOpts
        cursor
        R.defaultScanOpts {R.scanMatch = Just "ah3e:table:*", R.scanCount = Just 200}
        Nothing
    case res of
      Left _ -> pure acc
      Right (cursor', keys) -> do
        let acc' = acc <> mapMaybe (UUID.fromASCIIBytes <=< BS.stripPrefix "ah3e:table:") keys
        if cursor' == R.cursor0 then pure acc' else scanTables cursor' acc'
  bring tid = do
    here <- store.getTable tid
    case here of
      Just _ -> pure False
      Nothing ->
        R.runRedis conn (R.get ("ah3e:table:" <> UUID.toASCIIBytes tid)) >>= \case
          Right (Just raw) | Just t <- decodeJson raw -> True <$ store.insertTable t
          _ -> pure False
