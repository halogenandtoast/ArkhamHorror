{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Headless ML extractor (arkham-extract).
--
-- Replays stored Arkham Horror games out of the (read-only) Postgres DB and
-- emits a learning-to-rank training dataset as JSONL: one row per /choice/ per
-- /human decision/. Each row carries the 67 stable 'choiceFeatures' keys, the
-- 10 'scoreBreakdown' terms, a @chosen@ 0/1 label (the index the human actually
-- picked), a @group_id@ (= @game_id:step@) tying the candidate choices of one
-- decision together, and a per-game @outcome@ stamp for later outcome-weighting.
--
-- State reconstruction, answer handling, and the feature functions are all
-- reused from the arkham-api library; this tool only orchestrates them.
module Main where

import Api.Arkham.Helpers (GameApp (..), gameIdToText, runGameApp)
import Arkham.Ai.Decision (choiceFeatures, flattenChoices, gatherSituation, scoreBreakdown, unwrapQuestion)
import Arkham.Ai.State (aiEnabled, defaultAiPlayerState)
import Arkham.Classes.Entity (attr, toId)
import Arkham.Classes.HasQueue (newQueue)
import Arkham.Game (Game (..), runMessages)
import Arkham.Game.Diff (patchValueWithRecovery)
import Arkham.Game.Settings (settingsAiPlayers)
import Arkham.Game.State (GameState (IsOver))
import Arkham.Id (InvestigatorId, PlayerId, unInvestigatorId)
import Arkham.Investigator.Types
  ( InvestigatorAttrs
      ( investigatorDefeated
      , investigatorDrivenInsane
      , investigatorKilled
      , investigatorPlayerId
      , investigatorResigned
      )
  )
import Arkham.Message (Message (ClearUI, SetActivePlayer))
import Arkham.Queue (queueToRef)
import Arkham.Question (UI)
import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (foldM, forM, forM_, when)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Random (mkStdGen)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson (Value (Array, Null, Object), encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Diff (Patch)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Result (..))
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Functor.Identity (runIdentity)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (find, nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Database.Persist
  ( SelectOpt (Asc, LimitTo)
  , entityVal
  , get
  , selectKeysList
  , selectList
  , (<=.)
  , (==.)
  , (>=.)
  )
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Entity.Answer (Answer (Answer), QuestionResponse (..), Reply (Handled, Unhandled), handleAnswerPure)
import Entity.Arkham.Game (ArkhamGameId, arkhamGameCurrentData)
import Entity.Arkham.Game qualified as G
import Entity.Arkham.Step
  ( ActionDiff (..)
  , ArkhamStep
  , Choice (..)
  , arkhamStepActionDiff
  , arkhamStepChoice
  , arkhamStepStep
  )
import Entity.Arkham.Step qualified as S
import OpenTelemetry.Trace
  ( Tracer
  , detectInstrumentationLibrary
  , initializeGlobalTracerProvider
  , makeTracer
  , tracerOptions
  )
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
import UnliftIO.Async (pooledForConcurrentlyN)
import UnliftIO.MVar (MVar, newMVar, withMVar)
import Prelude

-- ---------------------------------------------------------------------------
-- CLI

data Opts = Opts
  { optDb :: Maybe String
  , optOut :: FilePath
  , optLimit :: Maybe Int
  , optJobs :: Int
  , optDebug :: Bool
  , optMaxSteps :: Maybe Int
  , optMinSteps :: Maybe Int
  }

defaultOpts :: Opts
defaultOpts = Opts Nothing "arkham-ml.jsonl" Nothing 1 False Nothing Nothing

usage :: String
usage =
  unlines
    [ "Usage: arkham-extract [--db CONNSTR] [--out FILE] [--limit N] [--jobs K] [--debug]"
    , ""
    , "  --db CONNSTR   Postgres connection string (default: $DATABASE_URL)."
    , "                 Read-only; the tool never writes to the DB."
    , "  --out FILE     JSONL output path (default: arkham-ml.jsonl)."
    , "  --limit N      Process only the first N games (ordered by game id)."
    , "  --jobs K       Process K games in parallel (default 1)."
    , "  --debug        For the first few no-match decisions, dump the top differing"
    , "                 normalized JSON paths (candidate 0 vs target) to stderr."
    , "  --max-steps N  Skip games with more than N steps (the slow long tail)."
    , "  --min-steps N  Skip games with fewer than N steps (abandoned setup stubs)."
    ]

parseArgs :: [String] -> IO Opts
parseArgs = go defaultOpts
 where
  go o [] = pure o
  go _ ("--help" : _) = die usage
  go _ ("-h" : _) = die usage
  go o ("--db" : v : rest) = go o {optDb = Just v} rest
  go o ("--out" : v : rest) = go o {optOut = v} rest
  go o ("--debug" : rest) = go o {optDebug = True} rest
  go o ("--max-steps" : v : rest) = case reads v of
    [(n, "")] -> go o {optMaxSteps = Just n} rest
    _ -> die $ "--max-steps expects an integer, got: " <> v
  go o ("--min-steps" : v : rest) = case reads v of
    [(n, "")] -> go o {optMinSteps = Just n} rest
    _ -> die $ "--min-steps expects an integer, got: " <> v
  go o ("--limit" : v : rest) = case reads v of
    [(n, "")] -> go o {optLimit = Just n} rest
    _ -> die $ "--limit expects an integer, got: " <> v
  go o ("--jobs" : v : rest) = case reads v of
    [(n, "")] | n >= 1 -> go o {optJobs = n} rest
    _ -> die $ "--jobs expects a positive integer, got: " <> v
  go _ (x : _) = die $ "Unexpected argument: " <> x <> "\n" <> usage

-- ---------------------------------------------------------------------------
-- Counters

-- | Per-run tallies. A 'Monoid' so each game's counts fold up trivially.
data Counts = Counts
  { cGames :: !Int
  , cFailures :: !Int
  , cDecisions :: !Int
  , cEmitted :: !Int
  , cRows :: !Int
  , cSkipAI :: !Int
  , cSkipNonIndex :: !Int
  , cSkipNoMatch :: !Int
  , cSkipAmbiguous :: !Int
  }

instance Semigroup Counts where
  a <> b =
    Counts
      (cGames a + cGames b)
      (cFailures a + cFailures b)
      (cDecisions a + cDecisions b)
      (cEmitted a + cEmitted b)
      (cRows a + cRows b)
      (cSkipAI a + cSkipAI b)
      (cSkipNonIndex a + cSkipNonIndex b)
      (cSkipNoMatch a + cSkipNoMatch b)
      (cSkipAmbiguous a + cSkipAmbiguous b)

instance Monoid Counts where
  mempty = Counts 0 0 0 0 0 0 0 0 0

-- | What the recorded step transition demands a correct candidate reproduce.
data Targets = Targets
  { tNext :: Value
  -- ^ normalized reconstructed next state S_k
  , tQueue :: Value
  -- ^ recorded leftover queue (JSON of choiceMessages_k)
  , tActionDiff :: Value
  -- ^ recorded engine action diff (JSON of arkhamStepActionDiff_k)
  }

-- | The product of re-executing one candidate index (already drained).
data ReExec = ReExec
  { reNext :: Value
  -- ^ normalized re-executed next state
  , reQueue :: Value
  -- ^ JSON of the leftover queue
  , reActionDiff :: Value
  -- ^ JSON of the re-executed game's gameActionDiff
  }

-- | Debug budget: a shared remaining-dump counter, present only under --debug.
type Dbg = Maybe (IORef Int)

-- ---------------------------------------------------------------------------
-- main

main :: IO ()
main = do
  opts <- parseArgs =<< getArgs
  connStr <- resolveConnStr (optDb opts)

  provider <- initializeGlobalTracerProvider
  let tracer = makeTracer provider $(detectInstrumentationLibrary) tracerOptions
      jobs = max 1 (optJobs opts)

  pool <- runNoLoggingT $ createPostgresqlPool connStr jobs

  gameIds <-
    runSqlPool
      ( selectKeysList
          ( maybe [] (\n -> [G.ArkhamGameStep <=. n]) (optMaxSteps opts)
              <> maybe [] (\n -> [G.ArkhamGameStep >=. n]) (optMinSteps opts)
          )
          (Asc G.ArkhamGameStep : maybe [] (pure . LimitTo) (optLimit opts))
      )
      pool

  hPutStrLn stderr $ "arkham-extract: " <> show (length gameIds) <> " games to process"

  dbg <- if optDebug opts then Just <$> newIORef (20 :: Int) else pure Nothing

  withFile (optOut opts) WriteMode $ \h -> do
    hSetBuffering h LineBuffering
    outLock <- newMVar ()
    let sink = mkSink h outLock
    perGame <-
      pooledForConcurrentlyN jobs gameIds $ \gid ->
        processGame dbg tracer pool sink gid
          `catchAny` \e -> do
            hPutStrLn stderr $ "game " <> T.unpack (gameIdToText gid) <> " FAILED: " <> show e
            pure mempty {cFailures = 1, cGames = 1}
    printSummary (mconcat perGame)
 where
  catchAny :: IO a -> (SomeException -> IO a) -> IO a
  catchAny act h = either h pure =<< try act

resolveConnStr :: Maybe String -> IO ConnectionString
resolveConnStr (Just s) = pure (BSC.pack s)
resolveConnStr Nothing =
  lookupEnv "DATABASE_URL" >>= \case
    Just s -> pure (BSC.pack s)
    Nothing -> die "No database connection: pass --db CONNSTR or set DATABASE_URL"

printSummary :: Counts -> IO ()
printSummary c = do
  hPutStrLn stderr "---- arkham-extract summary ----"
  forM_
    [ ("games processed", cGames c)
    , ("  of which failed", cFailures c)
    , ("decisions examined", cDecisions c)
    , ("decisions emitted", cEmitted c)
    , ("rows written", cRows c)
    , ("skipped: AI seat", cSkipAI c)
    , ("skipped: non-index question", cSkipNonIndex c)
    , ("skipped: no candidate matched", cSkipNoMatch c)
    , ("skipped: ambiguous (>1 matched)", cSkipAmbiguous c)
    ]
    $ \(label, n) -> hPutStrLn stderr $ "  " <> label <> ": " <> show n

-- | The output sink: write a decision's rows under the shared handle lock. One
-- lock acquisition per decision keeps a decision's rows contiguous and bounds
-- contention to the decision count (engine drains dominate the cost anyway).
type Sink = [Value] -> IO ()

mkSink :: Handle -> MVar () -> Sink
mkSink _ _ [] = pure ()
mkSink h lock rows =
  withMVar lock $ \_ -> forM_ rows $ \r -> BL8.hPutStrLn h (encode r)

-- ---------------------------------------------------------------------------
-- Per-game processing

-- | Load the final game + its steps, derive the outcome once (from the final
-- state), then reconstruct and label each decision. Read-only.
processGame :: Dbg -> Tracer -> ConnectionPool -> Sink -> ArkhamGameId -> IO Counts
processGame dbg tracer pool sink gid = do
  mGame <- runSqlPool (get gid) pool
  case mGame of
    Nothing -> pure mempty {cGames = 1}
    Just ge -> do
      let finalGame = arkhamGameCurrentData ge
          outcome = deriveOutcome finalGame
      steps <-
        runSqlPool
          (selectList [S.ArkhamStepArkhamGameId ==. gid] [Asc S.ArkhamStepStep])
          pool
      cnt <- reconstruct dbg tracer gid sink outcome finalGame (map entityVal steps)
      pure (cnt <> mempty {cGames = 1})

-- ---------------------------------------------------------------------------
-- Decision reconstruction (mirrors Api.Handler.Arkham.Replay's undo fold)
--
-- current_data is S_maxStep. The step record at step k stores
-- choicePatchDown_k = diff S_k S_{k-1}; applying it to S_k yields S_{k-1}.
-- Folding choicePatchDown from current_data by descending step thus
-- reconstructs every historical state. The decision that produced step k was
-- made in state S_{k-1} (whose 'gameQuestion' is the parked question), so we
-- process the decision against the reconstructed S_{k-1}.

reconstruct :: Dbg -> Tracer -> ArkhamGameId -> Sink -> Text -> Game -> [ArkhamStep] -> IO Counts
reconstruct dbg tracer gid sink outcome finalGame stepRecs = do
  let descending = sortOn (Down . arkhamStepStep) stepRecs
  -- 'Just sKValue' while we can still unwind; once a patch fails it becomes
  -- 'Nothing' and every remaining (deeper) step is skipped.
  (_, counts) <- foldM step (Just (toJSON finalGame), mempty) descending
  pure counts
 where
  byStepMap :: Map Int ArkhamStep
  byStepMap = Map.fromList [(arkhamStepStep s, s) | s <- stepRecs]

  unAD (ActionDiff ps) = ps

  step :: (Maybe Value, Counts) -> ArkhamStep -> IO (Maybe Value, Counts)
  step (Nothing, acc) _ = pure (Nothing, acc)
  step (Just sKValue, acc) rec = do
    let k = arkhamStepStep rec
    case patchValueWithRecovery sKValue (choicePatchDown (arkhamStepChoice rec)) of
      Error _ -> pure (Nothing, acc) -- can't unwind past here; stop walking back
      Success sK1Value -> do
        -- The continuation appended after the answer messages (updateGame's
        -- `currentQueue`) is the queue stored on step (k-1).
        let prev = Map.lookup (k - 1) byStepMap
            continuation = maybe [] (choiceMessages . arkhamStepChoice) prev
            -- The faithful gameActionDiff the production handler loaded S_{k-1}
            -- with (= step (k-1)'s recorded ActionDiff). Injecting it makes the
            -- re-executed gameActionDiff reproduce the recorded one exactly,
            -- because handleActionDiff reconstructs the SAME action snapshot.
            startActionDiff = maybe [] (unAD . arkhamStepActionDiff) prev
            targets =
              Targets
                { tNext = normalizeGameValue sKValue
                , tQueue = toJSON (choiceMessages (arkhamStepChoice rec))
                , tActionDiff = toJSON (arkhamStepActionDiff rec)
                }
        dc <- decideStep dbg tracer gid sink outcome k sK1Value continuation startActionDiff targets
        pure (Just sK1Value, acc <> dc)

-- | Process one decision: identify the answering player + parked question,
-- recover the chosen index by re-execution, and (for a human, unambiguous
-- match) emit one feature row per choice.
decideStep
  :: Dbg
  -> Tracer
  -> ArkhamGameId
  -> Sink
  -> Text
  -> Int
  -- ^ step k
  -> Value
  -- ^ reconstructed S_{k-1}
  -> [Message]
  -- ^ resume continuation queue
  -> [Patch]
  -- ^ faithful start gameActionDiff (step k-1's recorded ActionDiff)
  -> Targets
  -> IO Counts
decideStep dbg tracer gid sink outcome k sN1Value continuation startActionDiff targets =
  case fromJSON sN1Value of
    Error _ -> pure mempty {cDecisions = 1, cSkipNoMatch = 1}
    Success (game0 :: Game) -> do
      -- Start the re-execution from the state the production handler actually
      -- loaded: S_{k-1} carrying its faithful gameActionDiff.
      let game = game0 {gameActionDiff = startActionDiff}
          aiSeats = settingsAiPlayers (gameSettings game)
          flattenables =
            [ (pid, cs)
            | (pid, q) <- Map.toList (gameQuestion game)
            , Just (cs, _off) <- [flattenChoices (unwrapQuestion q)]
            , not (null cs)
            ]
      if null flattenables
        then pure mempty {cDecisions = 1, cSkipNonIndex = 1}
        else do
          -- For every parked flattenable seat, re-execute every candidate index.
          perPlayer <-
            forM flattenables $ \(pid, cs) -> do
              res <- forM [0 .. length cs - 1] $ \j -> do
                mre <- reexecute tracer gid game pid j continuation
                pure (j, mre)
              pure (pid, cs, res)
          let matches =
                [ (pid, j, cs)
                | (pid, cs, res) <- perPlayer
                , (j, Just re) <- res
                , candidateMatches targets re
                ]
          case matches of
            [(pid, j, cs)] -> case Map.lookup pid aiSeats of
              Just st | aiEnabled st -> pure mempty {cDecisions = 1, cSkipAI = 1}
              _ -> do
                rows <- buildRows gid outcome k pid game cs j
                sink rows
                pure mempty {cDecisions = 1, cEmitted = 1, cRows = length rows}
            [] -> do
              debugNoMatch dbg gid k targets perPlayer
              pure mempty {cDecisions = 1, cSkipNoMatch = 1}
            _ -> pure mempty {cDecisions = 1, cSkipAmbiguous = 1}

-- | A candidate matches iff it leaves the recorded queue AND reproduces either
-- the recorded next state (normalized) or the recorded engine action diff (when
-- that diff is non-trivial — an empty action diff is not discriminating).
candidateMatches :: Targets -> ReExec -> Bool
candidateMatches t re =
  reQueue re == tQueue t
    && ( reNext re == tNext t
          || (actionDiffDiscriminating (tActionDiff t) && reActionDiff re == tActionDiff t)
       )

-- | Re-run candidate index @j@ for @answerPid@ exactly as
-- 'Api.Handler.Arkham.Games.Shared.updateGame' would, returning the drained
-- next state (normalized), the leftover queue, and the engine action diff.
-- 'Nothing' when the answer is unhandled or the drain throws.
reexecute :: Tracer -> ArkhamGameId -> Game -> PlayerId -> Int -> [Message] -> IO (Maybe ReExec)
reexecute tracer gid game answerPid j continuation = do
  let activePid = gameActivePlayerId game
      answer =
        Answer
          QuestionResponse
            { qrChoice = j
            , qrPlayerId = Just answerPid
            , qrQuestionVersion = Just (gameScenarioSteps game)
            }
  reply <- handleAnswerPure game answerPid answer
  case reply of
    Unhandled _ -> pure Nothing
    Handled answerMessages -> do
      let bracketed =
            [SetActivePlayer answerPid | activePid /= answerPid]
              <> answerMessages
              <> [SetActivePlayer activePid | activePid /= answerPid]
          queue = (ClearUI : bracketed) <> continuation
      gameRef <- newIORef game
      queueRef <- newQueue queue
      genRef <- newIORef (mkStdGen (gameSeed game))
      let app = GameApp gameRef queueRef genRef (const (pure ())) tracer Nothing
      -- Engine drains are not pure; force the JSON inside the guard so any
      -- thunked exception is caught here (→ non-match) rather than escaping.
      result <- try @SomeException $ do
        runGameApp app (runMessages (gameIdToText gid) Nothing)
        resultGame <- readIORef gameRef
        leftover <- readIORef (queueToRef queueRef)
        let re =
              ReExec
                { reNext = normalizeGameValue (toJSON resultGame)
                , reQueue = toJSON (leftover :: [Message])
                , reActionDiff = toJSON (gameActionDiff resultGame)
                }
        _ <- evaluate (force (reNext re, reQueue re, reActionDiff re))
        pure re
      pure (either (const Nothing) Just result)

-- ---------------------------------------------------------------------------
-- Row construction + features

-- | One JSONL row per flattened choice. Features/breakdown are read from the
-- decision-state snapshot in @ReaderT Game Identity@ (exactly how 'decideAi'
-- invokes 'gatherSituation'), so the dataset distils against the live scorer.
buildRows :: ArkhamGameId -> Text -> Int -> PlayerId -> Game -> [UI Message] -> Int -> IO [Value]
buildRows gid outcome k pid game cs chosen = do
  let code = maybe "00000" unInvestigatorId (resolveInvestigator pid game)
      sit = runIdentity (runReaderT (gatherSituation (defaultAiPlayerState code) pid) game)
      groupId = gameIdToText gid <> ":" <> T.pack (show k)
  pure
    [ object
        [ "game_id" .= gameIdToText gid
        , "step" .= k
        , "player_id" .= T.pack (show pid)
        , "group_id" .= groupId
        , "outcome" .= outcome
        , "chosen" .= (if i == chosen then (1 :: Int) else 0)
        , "features" .= object [K.fromText name .= v | (name, v) <- choiceFeatures sit ui]
        , "breakdown" .= object [K.fromText name .= v | (name, v) <- scoreBreakdown sit ui]
        ]
    | (i, ui) <- zip [0 ..] cs
    ]

-- | Resolve the seat's controlled investigator id in the decision snapshot
-- (only its card code is needed, to pick the focus profile).
resolveInvestigator :: PlayerId -> Game -> Maybe InvestigatorId
resolveInvestigator pid game =
  toId <$> find ((== pid) . attr investigatorPlayerId) (Map.elems (gameEntities game).investigators)

-- ---------------------------------------------------------------------------
-- Outcome derivation
--
-- A coarse, generic survival heuristic (true win/loss needs scenario-specific
-- resolution mapping, out of scope for a generic extractor):
--
--   * not IsOver            -> "unknown" (still in progress / parked)
--   * every seat eliminated -> "loss"    (all investigators killed or insane)
--   * some seats eliminated -> "partial"
--   * none eliminated       -> "win"     (everyone survived; resigned or done)

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

-- ---------------------------------------------------------------------------
-- State normalization
--
-- A fresh re-execution legitimately differs from the stored/reconstructed state
-- in RNG, window-machinery, mid-resolution, and undo-bookkeeping fields, none of
-- which is substantive board state. Zero them (to Null) on BOTH sides so the
-- comparison turns on the entities / question / modifiers / board only. The
-- discriminating signal is preserved: gameEntities, gameQuestion, gameModifiers,
-- gameCards, the entity maps, gameSkillTest, gameGameState, the active/lead/turn
-- ids, etc. are all kept, and the leftover queue is a separate strong gate.

normalizeGameValue :: Value -> Value
normalizeGameValue = stripLabels . zeroVolatile . canonicalize
 where
  -- The dump predates current JSON-format migrations (empty-vs-null, omitted
  -- defaults, bare-int vs {contents,tag} GameValues). Round-trip through the
  -- engine's own Game (de)serializer so the reconstructed (old-format) and
  -- re-executed (new-format) states land in ONE canonical format. Idempotent on
  -- the already-current re-executed side; migrates the stored side forward.
  canonicalize v = case fromJSON v of
    Success (g :: Game) -> toJSON g
    Error _ -> v
  zeroVolatile = \case
    Object o -> Object (foldr (\k -> KM.insert k Null) o volatileGameKeys)
    v -> v
  -- i18n display labels were stored expanded in the dump but are message keys
  -- now; they're display-only, never the substantive discriminator (the choice
  -- target + messages are), so null every "label" recursively.
  stripLabels = \case
    Object o -> Object (KM.mapWithKey (\k x -> if k == "label" then Null else stripLabels x) o)
    Array a -> Array (V.map stripLabels a)
    v -> v

-- | JSON keys of 'Arkham.Game.Base.Game' that are RNG / window-machinery /
-- mid-resolution / env / undo-bookkeeping, not substantive board state.
volatileGameKeys :: [K.Key]
volatileGameKeys =
  [ "gameActionDiff"
  , "gameGitRevision"
  , "gameSeed"
  , "gameWindowDepth"
  , "gameWindowStack"
  , "gameWindowTick"
  , "gameWindowTickStack"
  , "gameEntryTicks"
  , "gameRunWindows"
  , "gameDepthLock"
  , "gameIgnoreCanModifiers"
  , "gamePhaseStep"
  , "gameFocusedCards"
  , "gameFocusedTarotCards"
  , "gameFocusedChaosTokens"
  , "gameActiveCard"
  , "gameResolvingCard"
  , "gameActiveAbilities"
  , "gameActiveCost"
  , "gameActionRemovedEntities"
  , "gameActionCanBeUndone"
  , "gameCurrentBatchId"
  , "gameUndoActionStep"
  , "gameUndoTurnStep"
  , "gameUndoPhaseStep"
  , "gameUndoRoundStep"
  , "gameAsIfAtIgnored"
  , "gameEnemyMoving"
  , "gameEnemyEvading"
  ]

-- | An action diff is discriminating only if it carries at least one real
-- operation. @[]@ (gameInAction False) and @[[]]@ (snapshot == state) are not.
actionDiffDiscriminating :: Value -> Bool
actionDiffDiscriminating = \case
  Array patches -> any hasOps patches
  _ -> False
 where
  hasOps (Array ops) = not (V.null ops)
  hasOps _ = False

-- ---------------------------------------------------------------------------
-- Debug: structural value-diff for the first few no-match decisions

debugNoMatch :: Dbg -> ArkhamGameId -> Int -> Targets -> [(PlayerId, [UI Message], [(Int, Maybe ReExec)])] -> IO ()
debugNoMatch Nothing _ _ _ _ = pure ()
debugNoMatch (Just ref) gid k targets perPlayer = do
  slot <- atomicModifyIORef' ref $ \n -> (max 0 (n - 1), n)
  when (slot > 0) $ case perPlayer of
    [] -> pure ()
    ((pid, cs, res) : _) -> do
      hPutStrLn stderr $
        "[debug] no-match game "
          <> T.unpack (gameIdToText gid)
          <> " step "
          <> show k
          <> " player "
          <> show pid
          <> " ("
          <> show (length cs)
          <> " choices); targetActionDiff discriminating="
          <> show (actionDiffDiscriminating (tActionDiff targets))
      -- Prefer the candidate whose leftover queue matches the recorded queue:
      -- that is the likely-correct answer whose STATE still mismatches, so its
      -- diff is exactly the set of fields normalization may still need to cover.
      -- Fall back to candidate 0 if no candidate's queue matched.
      let queueMatched = [(j, re) | (j, Just re) <- res, reQueue re == tQueue targets]
      case queueMatched of
        ((j, re) : _) -> do
          hPutStrLn stderr $
            "  candidate "
              <> show j
              <> " (queue MATCHES): stateOk="
              <> show (reNext re == tNext targets)
              <> " actionDiffOk="
              <> show (reActionDiff re == tActionDiff targets)
          dumpStateDiff re
        [] -> do
          hPutStrLn stderr "  no candidate's leftover queue matched the recorded queue; showing candidate 0"
          case lookup 0 res of
            Just (Just re0) -> dumpStateDiff re0
            _ -> hPutStrLn stderr "  candidate 0: unhandled / threw during drain"
 where
  dumpStateDiff re =
    let diffs = take 40 (valueDiff "" (reNext re) (tNext targets))
     in if null diffs
          then hPutStrLn stderr "  (normalized state identical — mismatch is queue or actionDiff)"
          else forM_ diffs $ \(p, got, want) ->
            hPutStrLn stderr $ "    " <> T.unpack p <> ": got=" <> shortJson got <> " want=" <> shortJson want

-- | Recursive leaf-level structural diff: every path where @got@ and @want@
-- disagree, as @(path, got, want)@. Lazy, so a @take N@ caps the work.
valueDiff :: Text -> Value -> Value -> [(Text, Value, Value)]
valueDiff path got want
  | got == want = []
  | otherwise = case (got, want) of
      (Object og, Object ow) ->
        let ks = nub (map K.toText (KM.keys og) ++ map K.toText (KM.keys ow))
         in concatMap
              ( \k ->
                  valueDiff
                    (path <> "." <> k)
                    (fromMaybe Null (KM.lookup (K.fromText k) og))
                    (fromMaybe Null (KM.lookup (K.fromText k) ow))
              )
              ks
      (Array ag, Array aw)
        | V.length ag == V.length aw ->
            concat $
              zipWith3
                (\i x y -> valueDiff (path <> "[" <> T.pack (show i) <> "]") x y)
                [0 :: Int ..]
                (V.toList ag)
                (V.toList aw)
        | otherwise -> [(path <> "[length]", toJSON (V.length ag), toJSON (V.length aw))]
      _ -> [(path, got, want)]

shortJson :: Value -> String
shortJson = take 200 . BL8.unpack . encode
