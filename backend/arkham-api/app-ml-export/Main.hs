{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Headless ML exporter (arkham-ml-export).
--
-- Reads the (read-only) @arkham_ml_decisions@ table written LIVE by the API
-- ('Api.Handler.Arkham.Games.Shared.captureMlDecision'), joins each decision's
-- eventual game outcome (from @arkham_games.current_data@ via 'deriveOutcome'),
-- and UNPACKS each decision's denormalized @rows@ jsonb array into one JSONL
-- line per choice, in exactly the schema @ml/train.py@ consumes:
--
-- @
--   { "game_id", "step", "player_id",
--     "group_id" (= game_id:step), "outcome",
--     "chosen" (0|1), "features": {67}, "breakdown": {10} }
-- @
--
-- The capture already computed the (drift-free) features against the live
-- engine, so this tool does no engine work beyond deriving the per-game outcome;
-- the feature functions are never re-run.
module Main where

import Api.Arkham.Helpers (gameIdToText)
import Arkham.Classes.Entity (attr)
import Arkham.Game (Game (..))
import Arkham.Game.State (GameState (IsOver))
import Arkham.Investigator.Types
  ( InvestigatorAttrs
      ( investigatorDefeated
      , investigatorDrivenInsane
      , investigatorKilled
      , investigatorResigned
      )
  )
import Control.Monad (foldM, forM_)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (Value (Array, Bool, Object), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Database.Persist
  ( Entity (..)
  , SelectOpt (Asc, LimitTo)
  , get
  , selectList
  )
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Entity.Arkham.Game (ArkhamGameId, arkhamGameCurrentData)
-- Imported wholesale so the generated 'EntityField' constructor
-- 'ArkhamMlDecisionCreatedAt' (a data-family constructor) and the record
-- accessors are all in scope without explicit-list syntax.
import Entity.Arkham.MlDecision
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.IO
  ( BufferMode (LineBuffering)
  , Handle
  , IOMode (WriteMode)
  , hPutStrLn
  , hSetBuffering
  , stderr
  , withFile
  )
import Prelude

-- ---------------------------------------------------------------------------
-- CLI

data Opts = Opts
  { optDb :: Maybe String
  , optOut :: FilePath
  , optLimit :: Maybe Int
  }

defaultOpts :: Opts
defaultOpts = Opts Nothing "arkham-ml.jsonl" Nothing

usage :: String
usage =
  unlines
    [ "Usage: arkham-ml-export [--db CONNSTR] [--out FILE] [--limit N]"
    , ""
    , "  --db CONNSTR   Postgres connection string (default: $DATABASE_URL)."
    , "                 Read-only; the tool never writes to the DB."
    , "  --out FILE     JSONL output path (default: arkham-ml.jsonl)."
    , "  --limit N      Process only the first N decisions (oldest first)."
    ]

parseArgs :: [String] -> IO Opts
parseArgs = go defaultOpts
 where
  go o [] = pure o
  go _ ("--help" : _) = die usage
  go _ ("-h" : _) = die usage
  go o ("--db" : v : rest) = go o {optDb = Just v} rest
  go o ("--out" : v : rest) = go o {optOut = v} rest
  go o ("--limit" : v : rest) = case reads v of
    [(n, "")] -> go o {optLimit = Just n} rest
    _ -> die $ "--limit expects an integer, got: " <> v
  go _ (x : _) = die $ "Unexpected argument: " <> x <> "\n" <> usage

resolveConnStr :: Maybe String -> IO ConnectionString
resolveConnStr (Just s) = pure (BSC.pack s)
resolveConnStr Nothing =
  lookupEnv "DATABASE_URL" >>= \case
    Just s -> pure (BSC.pack s)
    Nothing -> die "No database connection: pass --db CONNSTR or set DATABASE_URL"

-- ---------------------------------------------------------------------------
-- main

main :: IO ()
main = do
  opts <- parseArgs =<< getArgs
  connStr <- resolveConnStr (optDb opts)
  -- A single connection is enough: decisions are read once (materialized,
  -- bounded by --limit), then writes are streamed line by line while per-game
  -- outcomes are looked up (memoized) on the same connection.
  pool <- runNoLoggingT $ createPostgresqlPool connStr 1
  cacheRef <- newIORef Map.empty
  withFile (optOut opts) WriteMode $ \h -> do
    hSetBuffering h LineBuffering
    decisions <-
      runSqlPool
        ( selectList
            []
            (Asc ArkhamMlDecisionCreatedAt : maybe [] (pure . LimitTo) (optLimit opts))
        )
        pool
    total <- foldM (writeDecision h pool cacheRef) (0 :: Int) decisions
    hPutStrLn stderr $
      "arkham-ml-export: wrote "
        <> show total
        <> " rows from "
        <> show (length decisions)
        <> " decisions"

-- | Stream one decision's choices to the handle, resolving (and memoizing) the
-- game outcome. Returns the running row count.
writeDecision
  :: Handle
  -> ConnectionPool
  -> IORef (Map ArkhamGameId Text)
  -> Int
  -> Entity ArkhamMlDecision
  -> IO Int
writeDecision h pool cacheRef n (Entity _ d) = do
  outcome <- gameOutcome pool cacheRef (arkhamMlDecisionArkhamGameId d)
  let ls = explodeDecision d outcome
  forM_ ls $ \v -> BL8.hPutStrLn h (encode v)
  pure (n + length ls)

-- | The decision's eventual game outcome, memoized per game id. Only games
-- actually referenced get loaded, and each at most once (decisions are ordered
-- so same-game rows cluster). A missing game is "unknown".
gameOutcome :: ConnectionPool -> IORef (Map ArkhamGameId Text) -> ArkhamGameId -> IO Text
gameOutcome pool cacheRef gid = do
  cache <- readIORef cacheRef
  case Map.lookup gid cache of
    Just o -> pure o
    Nothing -> do
      mGame <- runSqlPool (get gid) pool
      let o = maybe "unknown" (deriveOutcome . arkhamGameCurrentData) mGame
      modifyIORef' cacheRef (Map.insert gid o)
      pure o

-- ---------------------------------------------------------------------------
-- Row explosion
--
-- The stored `rows` is [{ "chosen": bool, "features": {..}, "breakdown": {..} }];
-- emit one JSONL line per element in the exact ml/train.py schema, converting
-- the per-choice boolean `chosen` to the 0/1 label train.py expects.

explodeDecision :: ArkhamMlDecision -> Text -> [Value]
explodeDecision d outcome = case arkhamMlDecisionRows d of
  Array arr -> map toLine (V.toList arr)
  _ -> []
 where
  gameIdT = gameIdToText (arkhamMlDecisionArkhamGameId d)
  step = arkhamMlDecisionStep d
  pid = arkhamMlDecisionPlayerId d
  groupId = gameIdT <> ":" <> T.pack (show step)
  toLine choice =
    object
      [ "game_id" .= gameIdT
      , "step" .= step
      , "player_id" .= pid
      , "group_id" .= groupId
      , "outcome" .= outcome
      , "chosen" .= (if choiceChosen choice then (1 :: Int) else 0)
      , "features" .= objField "features" choice
      , "breakdown" .= objField "breakdown" choice
      ]
  choiceChosen = \case
    Object o -> case KM.lookup "chosen" o of
      Just (Bool b) -> b
      _ -> False
    _ -> False
  objField k = \case
    Object o -> fromMaybe (Object KM.empty) (KM.lookup k o)
    _ -> Object KM.empty

-- ---------------------------------------------------------------------------
-- Outcome derivation (mirrors arkham-extract):
--
--   * not IsOver            -> "unknown" (still in progress / parked)
--   * every seat eliminated -> "loss"
--   * some seats eliminated -> "partial"
--   * none eliminated       -> "win"

deriveOutcome :: Game -> Text
deriveOutcome game
  | gameGameState game /= IsOver = "unknown"
  | null invs = "unknown"
  | all eliminated invs = "loss"
  | any eliminated invs = "partial"
  | otherwise = "win"
 where
  invs = Map.elems (gameEntities game).investigators
  eliminated i =
    attr investigatorKilled i
      || attr investigatorDrivenInsane i
      || (attr investigatorDefeated i && not (attr investigatorResigned i))
