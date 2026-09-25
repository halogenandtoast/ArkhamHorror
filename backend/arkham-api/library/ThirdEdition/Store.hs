{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Where third-edition tables live. Not the database: in Redis when the app
has one (production runs several pods, so a table must be visible from all of
them), otherwise in this process. Either way a table outlives a page refresh,
and in Redis it also outlives a deploy until it sits idle for 'tableTtl'.

Each table keeps a short history of earlier game states for undo.
-}
module ThirdEdition.Store (
  Store (..),
  HistoryChange (..),
  newMemoryStore,
  newRedisStore,
) where

import AH3e.Game (Game)
import Control.Concurrent.MVar (modifyMVar)
import Control.Exception (throwIO)
import Data.Aeson (FromJSON, decodeStrict, encode)
import Data.List ((\\))
import Data.Map.Strict qualified as Map
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.Persist.Sql (fromSqlKey)
import Database.Redis (Connection, Queued, RedisTx, TxResult (..))
import Database.Redis qualified as R
import Model (UserId)
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

-- | An idle table is dropped after two weeks.
tableTtl :: Integer
tableTtl = 60 * 60 * 24 * 14

newMemoryStore :: IO Store
newMemoryStore = do
  var <- newMVar (mempty :: Map UUID (Table, [Game]))
  let withTables :: (Map UUID (Table, [Game]) -> IO (Map UUID (Table, [Game]), a)) -> IO a
      withTables = modifyMVar var
  pure
    Store
      { getTable = \tid -> fmap (second length) . Map.lookup tid <$> readMVar var
      , insertTable = \t -> withTables \m -> pure (Map.insert t.id (t, []) m, ())
      , updateTable = \tid f -> withTables \m -> case Map.lookup tid m of
          Nothing -> pure (m, Left "No such table")
          Just (t, hist) -> case f t (viaNonEmpty head hist) of
            Left e -> pure (m, Left e)
            Right (t', change) -> do
              let hist' = applyHistory change hist
              pure (Map.insert tid (t', hist') m, Right (t', length hist'))
      , deleteTable = \tid -> withTables \m -> pure (Map.delete tid m, ())
      , listTables = \uid -> do
          tables <- map fst . Map.elems <$> readMVar var
          pure ([t | t <- tables, not (started t)], [t | t <- tables, isSeated uid t])
      }
 where
  applyHistory = \case
    KeepHistory -> id
    PushHistory g -> take historyLimit . (g :)
    PopHistory -> drop 1

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
