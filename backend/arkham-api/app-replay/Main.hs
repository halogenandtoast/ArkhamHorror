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
import Arkham.Game (Game (..), runMessages)
import Arkham.Game.Diff (patchValueWithRecovery)
import Arkham.Message (Message (ClearUI, SetActivePlayer))
import Control.Monad (forM_, when)
import Control.Monad.Random (mkStdGen)
import Data.Aeson (Result (..), Value, eitherDecodeFileStrict', encode, fromJSON, toJSON)
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
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Prelude

data Opts = Opts
  { optExport :: FilePath
  , optAnswers :: Maybe FilePath
  , optOutput :: Maybe FilePath
  , optTrace :: Bool
  , optUndo :: Int
  }

defaultOpts :: Opts
defaultOpts = Opts "" Nothing Nothing False 0

usage :: String
usage =
  unlines
    [ "Usage: arkham-replay <export.json> [--undo N] [--answers answers.json] [--output out.json] [--trace]"
    , ""
    , "  <export.json>      Game export from /api/v1/arkham/games/:id/export"
    , "  --undo N           Step back N steps before resuming (applies choicePatchDown)"
    , "  --answers FILE     JSON list of Answer values to apply one at a time"
    , "                     (Answer accepts {\"tag\":\"Raw\",...}, {\"tag\":\"Answer\",...}, etc.)"
    , "  --output FILE      Write final Game state JSON here (default: stdout)"
    , "  --trace            Print every Message processed to stderr"
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

  -- Apply --undo: step back N steps by replaying their choicePatchDown patches
  -- (most-recent-step first). agedSteps is ordered by desc step (see
  -- generateExport), so the first optUndo entries are exactly the steps to undo.
  let stepsToUndo = take (optUndo opts) (sortOn (Down . arkhamStepStep) agedSteps)
  let targetStep = agedStep - length stepsToUndo
  currentData <-
    case foldl applyUndo (Right (toJSON agedCurrentData)) stepsToUndo of
      Left e -> die $ "undo failed: " <> e
      Right v -> case fromJSON v of
        Error e -> die $ "undo result deserialise failed: " <> e
        Success g -> pure (g :: Game)

  -- The queue waiting at the resume step.
  let resumeQueue =
        case filter ((== targetStep) . arkhamStepStep) agedSteps of
          (s : _) -> choiceMessages (arkhamStepChoice s)
          _ -> []

  provider <- initializeGlobalTracerProvider
  let tracer = makeTracer provider $(detectInstrumentationLibrary) tracerOptions

  let tracerCallback
        | optTrace opts = Just (\m -> hPutStrLn stderr ("> " <> show m))
        | otherwise = Nothing

  gameRef <- newIORef currentData
  queueRef <- newQueue resumeQueue
  genRef <- newIORef (mkStdGen currentData.gameSeed)
  let app = GameApp gameRef queueRef genRef (const (pure ())) tracer

  -- Drain any pending queue first, then process answers one at a time using
  -- the same dance as Api.Handler.Arkham.Games.Shared.updateGame: resolve the
  -- answer to a [Message] via handleAnswerPure, push it (bracketed by
  -- SetActivePlayer if the answering player isn't the active player), and
  -- run the queue.
  runGameApp app (runMessages "headless" tracerCallback)
  forM_ (zip [(0 :: Int) ..] answers) $ \(idx, ans) -> do
    g <- readIORef gameRef
    let activePid = gameActivePlayerId g
        answerPid = fromMaybe activePid (answerPlayer ans)
    handleAnswerPure g answerPid ans >>= \case
      Unhandled reason ->
        hPutStrLn stderr $
          "answer " <> show idx <> " unhandled: " <> T.unpack reason
      Handled msgs -> do
        let bracketed =
              [SetActivePlayer answerPid | activePid /= answerPid]
                <> msgs
                <> [SetActivePlayer activePid | activePid /= answerPid]
        runGameApp app (pushAll (ClearUI : bracketed))
        runGameApp app (runMessages "headless" tracerCallback)

  finalGame <- readIORef gameRef
  case optOutput opts of
    Nothing -> BL8.putStrLn (encode finalGame)
    Just f -> BSL.writeFile f (encode finalGame)

-- | Apply one step's choicePatchDown to the running JSON value. Stops on the
-- first failure.
applyUndo :: Either String Value -> ArkhamStep -> Either String Value
applyUndo (Left e) _ = Left e
applyUndo (Right v) step =
  case patchValueWithRecovery v (choicePatchDown (arkhamStepChoice step)) of
    Error e -> Left $ "step " <> show (arkhamStepStep step) <> ": " <> e
    Success v' -> Right v'
