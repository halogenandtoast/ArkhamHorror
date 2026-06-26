{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Headless replay CLI.
--
-- Loads a game export (from /api/v1/arkham/games/:id/export), optionally pushes
-- a list of raw 'Message's, runs the engine, and prints the resulting 'Game'.
-- No DB, no Yesod, no frontend. The point is to reproduce a bug in <1s instead
-- of the full investigate-bug stack.
module Main where

import Api.Arkham.Export
  ( ArkhamExport (..)
  , ArkhamGameExportData (..)
  )
import Api.Arkham.Helpers (GameApp (..), runGameApp)
import Arkham.Classes.HasQueue (newQueue, pushAll)
import Arkham.Game (Game (..), PublicGame (..), runMessages)
import Arkham.Game.Diff (diff, patchValueWithRecovery)
import Arkham.Game.Runner (handleActionDiff)
import Arkham.Message (Message (ClearUI, SetActivePlayer))
import Arkham.Metrics (dumpMetricsTo, enableMetrics, withMetric)
import Control.Exception (evaluate)
import GHC.Clock (getMonotonicTimeNSec)
import Control.Monad (forM, forM_, when)
import Control.Monad.Random (mkStdGen)
import Data.Aeson (Result (..), Value, eitherDecodeFileStrict', eitherDecode, encode, fromJSON, toJSON)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.IORef (newIORef, readIORef)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text qualified as T
import Entity.Answer (Reply (..), answerPlayer, handleAnswerPure)
import Entity.Arkham.Step (ArkhamStep (..), Choice (..))
import OpenTelemetry.Trace
  ( detectInstrumentationLibrary
  , initializeGlobalTracerProvider
  , makeTracer
  , tracerOptions
  )
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Prelude

data Opts = Opts
  { optExport :: FilePath
  , optAnswers :: Maybe FilePath
  , optOutput :: Maybe FilePath
  , optTrace :: Bool
  , optUndo :: Int
  , optMetrics :: Maybe (Maybe FilePath)
  -- ^ Outer Maybe = enabled?  Inner Maybe = where to dump (Nothing = stderr).
  , optMetricsTopN :: Int
  , optReplayAll :: Bool
  , optPerStepReport :: Maybe FilePath
  , optPerStepTopN :: Int
  , optSimulateServer :: Bool
  , optBenchActionDiff :: Int
  }

defaultOpts :: Opts
defaultOpts = Opts "" Nothing Nothing False 0 Nothing 50 False Nothing 30 False 0

usage :: String
usage =
  unlines
    [ "Usage: arkham-replay <export.json> [--undo N] [--answers answers.json] [--output out.json]"
    , "                                   [--trace] [--metrics [FILE]] [--metrics-top N]"
    , "                                   [--replay-all] [--per-step-report FILE] [--per-step-top N]"
    , ""
    , "  <export.json>      Game export from /api/v1/arkham/games/:id/export"
    , "  --undo N           Step back N steps before resuming (applies choicePatchDown)"
    , "  --answers FILE     JSON list of Answer values to apply one at a time"
    , "                     (Answer accepts {\"tag\":\"Raw\",...}, {\"tag\":\"Answer\",...}, etc.)"
    , "  --output FILE      Write final Game state JSON here (default: stdout)"
    , "  --trace            Print every Message processed to stderr"
    , "  --metrics [FILE]   Record per-span wall-clock timings; dump table to FILE (or stderr)"
    , "  --metrics-top N    Show top-N spans in the metrics table (default 50)"
    , "  --replay-all       Undo to step 0 then replay every step forward, timing each one."
    , "                     With --undo N, only undo N steps and replay those N forward."
    , "                     Top slowest steps are printed to stderr; combine with --metrics for"
    , "                     per-span breakdown across the whole replay."
    , "  --per-step-report FILE  Write step,duration_ms,server_ms,messages CSV (works with --replay-all)"
    , "  --per-step-top N   Show top-N slowest steps on stderr (default 30)"
    , "  --simulate-server  After each replayed step, perform the same JSON work the real"
    , "                     handler does per answer: force the accumulated action diffs,"
    , "                     compute the step's undo diff, encode the game for the DB write,"
    , "                     re-parse it (DB row load), and encode the PublicGame broadcast."
    , "                     Timings appear as server/* spans in --metrics and server_ms in"
    , "                     the per-step report."
    ]

parseArgs :: [String] -> IO Opts
parseArgs = go defaultOpts
 where
  go o [] = pure o
  go _ ("--help" : _) = die usage
  go _ ("-h" : _) = die usage
  go o ("--answers" : f : rest) = go o {optAnswers = Just f} rest
  go o ("--output" : f : rest) = go o {optOutput = Just f} rest
  go o ("--trace" : rest) = go o {optTrace = True} rest
  go o ("--undo" : n : rest) = case reads n of
    [(k, "")] -> go o {optUndo = k} rest
    _ -> die $ "--undo expects an integer, got: " <> n
  go o ("--metrics" : f : rest) | take 2 f /= "--" =
    go o {optMetrics = Just (Just f)} rest
  go o ("--metrics" : rest) = go o {optMetrics = Just Nothing} rest
  go o ("--metrics-top" : n : rest) = case reads n of
    [(k, "")] -> go o {optMetricsTopN = k} rest
    _ -> die $ "--metrics-top expects an integer, got: " <> n
  go o ("--replay-all" : rest) = go o {optReplayAll = True} rest
  go o ("--simulate-server" : rest) = go o {optSimulateServer = True} rest
  go o ("--bench-action-diff" : n : rest) = case reads n of
    [(k, "")] -> go o {optBenchActionDiff = k} rest
    _ -> die $ "--bench-action-diff expects an integer, got: " <> n
  go o ("--per-step-report" : f : rest) = go o {optPerStepReport = Just f} rest
  go o ("--per-step-top" : n : rest) = case reads n of
    [(k, "")] -> go o {optPerStepTopN = k} rest
    _ -> die $ "--per-step-top expects an integer, got: " <> n
  go o (x : rest)
    | optExport o == "" = go o {optExport = x} rest
    | otherwise = die $ "Unexpected argument: " <> x <> "\n" <> usage

main :: IO ()
main = do
  opts <- parseArgs =<< getArgs
  when (optExport opts == "") $ die usage

  ArkhamExport {aeCampaignData = ArkhamGameExportData {..}} <-
    either (die . ("Failed to parse export: " <>)) pure
      =<< eitherDecodeFileStrict' (optExport opts)

  answers <-
    case optAnswers opts of
      Nothing -> pure []
      Just f ->
        either (die . ("Failed to parse answers: " <>)) pure
          =<< eitherDecodeFileStrict' f

  -- Apply --undo (or full undo for --replay-all): step back N steps by
  -- replaying their choicePatchDown patches (most-recent-step first).
  -- agedSteps is ordered by desc step (see generateExport), so the first
  -- N entries are exactly the steps to undo.
  -- With --replay-all --undo N, only undo (and then replay) the last N steps;
  -- some exports cannot be unwound to step 0 (e.g. across scenario
  -- transitions), and a recent window is usually what you want to time anyway.
  -- For a full replay-all we unwind to the EARLIEST retained step, not past
  -- it. An export keeps only the most recent N steps, so unwinding all N lands
  -- at (minStep - 1) — a step that isn't retained and may predate the current
  -- scenario (the mode collapses to campaign-only `This`). Replaying forward
  -- then can't rebuild the scenario from queued messages (a campaign->scenario
  -- transition isn't message-reconstructable), so `scenarioField` throws. Stop
  -- one step short to resume from a retained, scenario-consistent state; that
  -- earliest step's queue is still executed as the resume queue below.
  let undoCount =
        if optReplayAll opts && optUndo opts == 0
          then max 0 (length agedSteps - 1)
          else optUndo opts
  let stepsToUndo = take undoCount (sortOn (Down . arkhamStepStep) agedSteps)
  let targetStep = agedStep - length stepsToUndo
  currentData <-
    case foldl applyUndo (Right (toJSON agedCurrentData)) stepsToUndo of
      Left e -> die $ "undo failed: " <> e
      Right v -> case fromJSON v of
        Error e -> die $ "undo result deserialise failed: " <> e
        Success g -> pure (g :: Game)

  -- Isolated benchmark of the in-action diff bookkeeping: run K minimal
  -- in-action messages through handleActionDiff on the loaded game, then
  -- force the accumulated gameActionDiff exactly like the per-answer save
  -- (ActionDiff $ view actionDiffL ge) does. This is the work a real save
  -- performs after K messages of an action have resolved.
  when (optBenchActionDiff opts > 0) $ do
    let k = optBenchActionDiff opts
    let g0 = currentData {gameInAction = True, gameActionDiff = []}
    t0 <- getMonotonicTimeNSec
    let stepG g i = handleActionDiff g (g {gameSeed = gameSeed g + i})
    let gk = foldl' stepG g0 [1 .. k]
    bytes <- evaluate (BSL.length (encode (gameActionDiff gk)))
    t1 <- getMonotonicTimeNSec
    hPutStrLn stderr
      $ "bench-action-diff: "
      <> show k
      <> " in-action messages; forcing the save cost took "
      <> printfMs (fromIntegral (t1 - t0) / 1_000_000)
      <> " ms ("
      <> show bytes
      <> " bytes of actionDiff JSON)"
    exitSuccess

  -- The queue waiting at the resume step.
  let resumeQueue =
        case filter ((== targetStep) . arkhamStepStep) agedSteps of
          (s : _) -> choiceMessages (arkhamStepChoice s)
          _ -> []

  provider <- initializeGlobalTracerProvider
  let tracer = makeTracer provider $(detectInstrumentationLibrary) tracerOptions

  metricsRef <- case optMetrics opts of
    Nothing -> pure Nothing
    Just _ -> Just <$> enableMetrics

  let tracerCallback
        | optTrace opts = Just (\m -> hPutStrLn stderr ("> " <> show m))
        | otherwise = Nothing

  gameRef <- newIORef currentData
  queueRef <- newQueue resumeQueue
  genRef <- newIORef (mkStdGen currentData.gameSeed)
  let app = GameApp gameRef queueRef genRef (const (pure ())) tracer Nothing

  -- Drain any pending queue first, then process answers one at a time using
  -- the same dance as Api.Handler.Arkham.Games.Shared.updateGame: resolve the
  -- answer to a [Message] via handleAnswerPure, push it (bracketed by
  -- SetActivePlayer if the answering player isn't the active player), and
  -- run the queue.
  wallStart <- getMonotonicTimeNSec
  runGameApp app (runMessages "headless" tracerCallback)

  perStepTimings <-
    if optReplayAll opts
      then do
        -- agedSteps in the export are sorted desc by step. Replay forward
        -- by ascending step index, pushing each step's choiceMessages and
        -- timing the resulting drain.
        let forwardSteps =
              sortOn arkhamStepStep
                $ filter ((> targetStep) . arkhamStepStep) agedSteps
        let total = length forwardSteps
        hPutStrLn stderr $ "Replaying " <> show total <> " steps forward..."
        timings <- forM (zip [(1 :: Int) ..] forwardSteps) $ \(idx, step) -> do
          let msgs = choiceMessages (arkhamStepChoice step)
          when (idx `mod` 100 == 0)
            $ hPutStrLn stderr ("  step " <> show idx <> "/" <> show total)
          gBefore <- readIORef gameRef
          runGameApp app (pushAll (ClearUI : msgs))
          t0 <- getMonotonicTimeNSec
          runGameApp app (runMessages "headless" tracerCallback)
          t1 <- getMonotonicTimeNSec
          serverNs <-
            if optSimulateServer opts
              then do
                ge <- readIORef gameRef
                s0 <- getMonotonicTimeNSec
                -- Mirror Api.Handler.Arkham.Games.Shared.updateGame, in order:
                -- 1. force the per-message action diffs saved into ArkhamStep
                _ <- withMetric "server/forceActionDiff" $ evaluate (BSL.length (encode (gameActionDiff ge)))
                -- 2. the step's undo patch (diff new state vs state at answer start)
                _ <- withMetric "server/diffDown" $ evaluate (BSL.length (encode (diff ge gBefore)))
                -- 3. encode the full game for the DB write (replace gameId g')
                gameBytes <- withMetric "server/encodeGame" $ do
                  let bs = encode ge
                  _ <- evaluate (BSL.length bs)
                  pure bs
                -- 4. parse the full game back (every answer re-reads the row)
                _ <- withMetric "server/parseGame" $ evaluate $ case eitherDecode @Game gameBytes of
                  Left e -> error ("simulate-server: game failed to re-parse: " <> e)
                  Right (g :: Game) -> gameSeed g
                -- 5. encode the PublicGame broadcast sent to every subscriber
                _ <-
                  withMetric "server/encodePublicGame"
                    $ evaluate (BSL.length (encode (PublicGame ("headless" :: T.Text) "bench" [] ge)))
                s1 <- getMonotonicTimeNSec
                pure (s1 - s0)
              else pure 0
          pure (arkhamStepStep step, t1 - t0, serverNs, length msgs)
        pure timings
      else do
        forM_ (zip [(0 :: Int) ..] answers) $ \(idx, ans) -> do
          g <- readIORef gameRef
          let activePid = gameActivePlayerId g
              answerPid = fromMaybe activePid (answerPlayer ans)
          handleAnswerPure g answerPid ans >>= \case
            Unhandled reason ->
              hPutStrLn stderr
                $ "answer "
                <> show idx
                <> " unhandled: "
                <> T.unpack reason
            Handled msgs -> do
              let bracketed =
                    [SetActivePlayer answerPid | activePid /= answerPid]
                      <> msgs
                      <> [SetActivePlayer activePid | activePid /= answerPid]
              runGameApp app (pushAll (ClearUI : bracketed))
              runGameApp app (runMessages "headless" tracerCallback)
        pure []

  wallEnd <- getMonotonicTimeNSec
  finalGame <- readIORef gameRef
  case optOutput opts of
    Nothing -> BL8.putStrLn (encode finalGame)
    Just f -> BSL.writeFile f (encode finalGame)

  case (optMetrics opts, metricsRef) of
    (Just dest, Just ref) -> do
      let elapsedMs = fromIntegral (wallEnd - wallStart) / (1_000_000 :: Double)
      hPutStrLn stderr
        $ "Replay wall-clock (excluding load + final encode): "
        <> show elapsedMs
        <> " ms"
      dumpMetricsTo dest ref (optMetricsTopN opts)
    _ -> pure ()

  when (not (null perStepTimings)) $ do
    let toMs ns = fromIntegral ns / (1_000_000 :: Double)
        totalNs (_, drainNs, serverNs, _) = drainNs + serverNs
        sortedDesc = sortOn (Down . totalNs) perStepTimings
        slowest = take (optPerStepTopN opts) sortedDesc
        sumDrain = sum [d | (_, d, _, _) <- perStepTimings]
        sumServer = sum [s | (_, _, s, _) <- perStepTimings]
    hPutStrLn stderr ""
    hPutStrLn stderr
      $ "Aggregate: drain "
      <> printfMs (toMs sumDrain)
      <> " ms, server-sim "
      <> printfMs (toMs sumServer)
      <> " ms over "
      <> show (length perStepTimings)
      <> " steps"
    hPutStrLn stderr "Top slowest steps (descending by drain+server time):"
    hPutStrLn stderr "  step      duration_ms     server_ms   messages_pushed"
    forM_ slowest $ \(step, ns, serverNs, msgs) ->
      hPutStrLn stderr
        $ "  "
        <> padLeft 8 (show step)
        <> "  "
        <> padLeft 11 (printfMs (toMs ns))
        <> "  "
        <> padLeft 12 (printfMs (toMs serverNs))
        <> "  "
        <> padLeft 5 (show msgs)
    case optPerStepReport opts of
      Nothing -> pure ()
      Just path -> do
        let rows =
              "step,duration_ms,server_ms,messages_pushed\n"
                <> concatMap
                  ( \(s, ns, serverNs, m) ->
                      show s
                        <> ","
                        <> printfMs (toMs ns)
                        <> ","
                        <> printfMs (toMs serverNs)
                        <> ","
                        <> show m
                        <> "\n"
                  )
                  perStepTimings
        writeFile path rows
        hPutStrLn stderr $ "Per-step CSV written to " <> path

-- | Apply one step's choicePatchDown to the running JSON value. Stops on the
-- first failure.
applyUndo :: Either String Value -> ArkhamStep -> Either String Value
applyUndo (Left e) _ = Left e
applyUndo (Right v) step =
  case patchValueWithRecovery v (choicePatchDown (arkhamStepChoice step)) of
    Error e -> Left $ "step " <> show (arkhamStepStep step) <> ": " <> e
    Success v' -> Right v'

padLeft :: Int -> String -> String
padLeft n s = replicate (max 0 (n - length s)) ' ' <> s

printfMs :: Double -> String
printfMs ms =
  let scaled = (round (ms * 100) :: Integer)
      whole = scaled `div` 100
      frac = scaled `mod` 100
   in show whole <> "." <> padFrac frac
 where
  padFrac f = (if f < 10 then "0" else "") <> show f
