{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Headless self-play win-rate harness (arkham-self-play).
--
-- Creates a SOLO Roland Banks game in The Gathering (Night of the Zealot)
-- entirely in memory, drives the AI decision engine ('Arkham.Ai.Decision.decideAi')
-- through a full game with no human and no database, and records win/loss/partial
-- per game. Running many games gives the AI's scenario win-rate — the quality gate
-- for any learned model and the baseline for the current heuristic.
--
-- == How the game is created (no DB)
--
-- We mirror the create-game handler's MINIMAL path
-- ('Api.Handler.Arkham.Games.postApiV1ArkhamGamesR'):
--
--   * 'newCampaign' "01" (Just "01104") — Night of the Zealot, started at The
--     Gathering. The CAMPAIGN path is required (not 'newScenario'): only the
--     campaign 'StartCampaign' uses @chooseDecksWithAi@, which loads an AI seat's
--     /bundled/ decklist in place instead of prompting for a saved deck. The
--     standalone-scenario path uses @chooseDecks@ (a deck prompt needing a DB row),
--     so it cannot run headless from a bundled deck.
--   * 'addPlayer' queues @StartCampaign@; 'RegisterAiPlayer' (pushed ahead of it)
--     registers the seat's 'AiPlayerState' so @StartCampaign@ resolves it to the
--     bundled @rolandCoreDeck@ ('Arkham.Ai.Decks.bundledDeckFor' "01001").
--   * 'runMessages' drains setup to the first parked question.
--
-- == The drive loop (per game, given a seed)
--
-- While 'gameGameState' /= 'IsOver': take the parked seat's 'gameQuestion',
-- compute an 'Answer', and apply it exactly as
-- 'Api.Handler.Arkham.Games.Shared.updateGame' does — re-seed the gen from the
-- parked game's 'gameSeed', push @ClearUI : answerMessages@ (bracketed by
-- 'SetActivePlayer' when the answering seat is not the active player) ahead of the
-- in-flight queue, and drain with 'runMessages'. Almost every answer comes from
-- 'decideAi' over the parked game (read-only, the same call the server makes for
-- AI seats); the one exception is the campaign-setup question
-- 'PickCampaignSettings' (and the standalone 'PickScenarioSettings'), which is not
-- an index question 'decideAi' can answer, so we feed an empty settings answer
-- directly (The Gathering is the first scenario — an empty campaign log is correct).
--
-- A MAX-STEPS safety cap aborts a stuck game (a 'decideAi' gap, an unhandled
-- answer, or a state that parks no question) so the harness can never hang, and any
-- thrown game is caught and counted as aborted rather than crashing the run.
module Main where

import Api.Arkham.Helpers (GameApp (..), runGameApp)
import Arkham.Ai.Decision (decideAi, unwrapQuestion)
import Arkham.Ai.State (AiPlayerState, defaultAiPlayerState)
import Arkham.Classes.Entity (attr)
import Arkham.Classes.HasQueue (newQueue, push, pushAll)
import Arkham.Difficulty (Difficulty (..))
import Arkham.Game (Game (..), addPlayer, newCampaign, runMessages)
import Arkham.Game.State (GameState (IsOver))
import Arkham.Id (CampaignId, PlayerId (..), ScenarioId)
import Arkham.Investigator.Types
  ( InvestigatorAttrs
      ( investigatorDefeated
      , investigatorDrivenInsane
      , investigatorKilled
      , investigatorResigned
      )
  )
import Arkham.Message (Message (ClearUI, RegisterAiPlayer, SetActivePlayer))
import Arkham.Question (Question (PickCampaignSettings, PickScenarioSettings))
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM)
import Control.Monad.Random (StdGen, mkStdGen)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.UUID qualified as UUID
import Entity.Answer
  ( Answer (CampaignSettingsAnswer, StandaloneSettingsAnswer)
  , CampaignSettings (CampaignSettings)
  , Reply (Handled, Unhandled)
  , handleAnswerPure
  )
import OpenTelemetry.Trace
  ( Tracer
  , detectInstrumentationLibrary
  , initializeGlobalTracerProvider
  , makeTracer
  , tracerOptions
  )
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import Prelude

-- ---------------------------------------------------------------------------
-- CLI

data Opts = Opts
  { optGames :: Int
  , optSeed :: Int
  , optScenario :: ScenarioId
  , optDifficulty :: Difficulty
  , optMaxSteps :: Int
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optGames = 1
    , optSeed = 0
    , optScenario = theGatheringId
    , optDifficulty = Easy
    , optMaxSteps = 5000
    }

usage :: String
usage =
  unlines
    [ "Usage: arkham-self-play [--games N] [--seed S] [--scenario SID]"
    , "                        [--difficulty easy|standard|hard|expert] [--max-steps N]"
    , ""
    , "  Plays N solo Roland Banks games (Night of the Zealot) headless, driving"
    , "  the AI decision engine to completion, and reports the scenario win-rate."
    , ""
    , "  --games N        Number of games to play (default 1). Seeds are S .. S+N-1."
    , "  --seed S         Base RNG seed (default 0); game i uses seed S+i."
    , "  --scenario SID   Night-of-the-Zealot scenario step id (default 01104, The"
    , "                   Gathering). Other ids must belong to campaign 01."
    , "  --difficulty D   easy | standard | hard | expert (default easy)."
    , "  --max-steps N    Per-game decision cap before a stuck game is counted"
    , "                   aborted (default 5000)."
    ]

parseArgs :: [String] -> IO Opts
parseArgs = go defaultOpts
 where
  go o [] = pure o
  go _ ("--help" : _) = die usage
  go _ ("-h" : _) = die usage
  go o ("--games" : v : rest) = case reads v of
    [(n, "")] | n >= 1 -> go o {optGames = n} rest
    _ -> die $ "--games expects a positive integer, got: " <> v
  go o ("--seed" : v : rest) = case reads v of
    [(n, "")] -> go o {optSeed = n} rest
    _ -> die $ "--seed expects an integer, got: " <> v
  go o ("--scenario" : v : rest) = go o {optScenario = fromString v} rest
  go o ("--max-steps" : v : rest) = case reads v of
    [(n, "")] | n >= 1 -> go o {optMaxSteps = n} rest
    _ -> die $ "--max-steps expects a positive integer, got: " <> v
  go o ("--difficulty" : v : rest) = case parseDifficulty v of
    Just d -> go o {optDifficulty = d} rest
    Nothing -> die $ "--difficulty expects easy|standard|hard|expert, got: " <> v
  go _ (x : _) = die $ "Unexpected argument: " <> x <> "\n" <> usage

parseDifficulty :: String -> Maybe Difficulty
parseDifficulty = \case
  "easy" -> Just Easy
  "standard" -> Just Standard
  "hard" -> Just Hard
  "expert" -> Just Expert
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- Fixed harness identities

-- | Night of the Zealot. Its 'StartCampaign' is the only path that loads an AI
-- seat's bundled deck (via @chooseDecksWithAi@), which is what makes the game
-- creatable headless with no saved deck.
nightOfTheZealotId :: CampaignId
nightOfTheZealotId = "01"

-- | The Gathering (the first NotZ scenario).
theGatheringId :: ScenarioId
theGatheringId = "01104"

-- | Roland Banks profile code — the one investigator the AI MVP ships a bundled
-- deck for ('Arkham.Ai.Decks.rolandCoreDeck' via @bundledDeckFor "01001"@). Both
-- the registered seat and the live 'decideAi' profile use it.
rolandAiState :: AiPlayerState
rolandAiState = defaultAiPlayerState "01001"

-- | One stable, non-nil seat id for the solo investigator. The game is rebuilt
-- fresh per run, so a fixed id is fine.
harnessPlayerId :: PlayerId
harnessPlayerId = PlayerId (UUID.fromWords 0x10010000 0x00000000 0x00000000 0x00000001)

-- ---------------------------------------------------------------------------
-- Outcome

data Outcome = Win | Loss | Partial | Aborted
  deriving stock (Eq, Show)

{- | The coarse survival heuristic mirrored from app-extract's @deriveOutcome@,
plus an 'Aborted' bucket for any game that did not reach 'IsOver' (max-steps cap,
a stuck no-question state, an unhandled answer, or a thrown exception). For a real
win\/loss\/partial the game must be over:

  * every seat eliminated -> 'Loss'
  * some seats eliminated  -> 'Partial'
  * none eliminated        -> 'Win' (everyone survived; resigned or resolved)
-}
deriveOutcome :: Game -> Outcome
deriveOutcome game
  | gameGameState game /= IsOver = Aborted
  | null invs = Aborted
  | all eliminated invs = Loss
  | any eliminated invs = Partial
  | otherwise = Win
 where
  invs = Map.elems (gameEntities game).investigators
  eliminated i =
    attr investigatorKilled i
      || attr investigatorDrivenInsane i
      || (attr investigatorDefeated i && not (attr investigatorResigned i))

-- ---------------------------------------------------------------------------
-- Per-game self-play

{- | Construct, set up, and fully drive one solo Roland \/ The Gathering game for
the given seed, returning its 'Outcome'. Any thrown exception (engine partial
function, etc.) is caught and reported as 'Aborted', so one poison game never
crashes the run.
-}
runOneGame :: Tracer -> ScenarioId -> Difficulty -> Int -> Int -> IO Outcome
runOneGame tracer scenario difficulty maxSteps seed = do
  result <- try @SomeException $ do
    -- Mirror postApiV1ArkhamGamesR's minimal in-memory creation. playerCount = 1
    -- (solo); includeTarotReadings = False (no tarot setup prompt).
    let game0 = newCampaign nightOfTheZealotId (Just scenario) seed 1 difficulty False
    gameRef <- newIORef game0
    queueRef <- newQueue []
    genRef <- newIORef (mkStdGen seed)
    let app = GameApp gameRef queueRef genRef (const (pure ())) tracer Nothing

    -- Setup: addPlayer queues [StartCampaign]; RegisterAiPlayer (pushed ahead of
    -- it) makes StartCampaign load the bundled deck via chooseDecksWithAi. Drain
    -- to the first parked question (PickCampaignSettings).
    runGameApp app $ do
      addPlayer harnessPlayerId
      push (RegisterAiPlayer harnessPlayerId rolandAiState)
      runMessages "self-play-setup" Nothing

    outcome <- driveLoop app gameRef genRef maxSteps 0
    evaluate outcome
  pure (either (const Aborted) id result)

{- | The answer-apply loop, one parked question per step, applied exactly like
'updateGame'. Stops at 'IsOver' (real outcome) or when the safety cap \/ a stuck
state \/ an unhandled answer forces 'Aborted'.
-}
driveLoop :: GameApp -> IORef Game -> IORef StdGen -> Int -> Int -> IO Outcome
driveLoop app gameRef genRef maxSteps = loop
 where
  loop steps = do
    game <- readIORef gameRef
    if gameGameState game == IsOver
      then pure (deriveOutcome game)
      else
        if steps >= maxSteps
          then pure Aborted
          else case parkedQuestion game of
            Nothing -> pure Aborted -- not over, but nothing to answer: stuck
            Just (qpid, q) -> do
              -- Mirror updateGame: each action re-seeds the gen from the parked
              -- game's evolving gameSeed (toExternalGame draws a fresh seed at every
              -- park), so a given starting seed replays deterministically.
              writeIORef genRef (mkStdGen (gameSeed game))
              answer <- decideAnswer app qpid q
              reply <- handleAnswerPure game qpid answer
              case reply of
                Unhandled _ -> pure Aborted
                Handled answerMessages -> do
                  let activePid = gameActivePlayerId game
                      bracketed =
                        [SetActivePlayer qpid | activePid /= qpid]
                          <> answerMessages
                          <> [SetActivePlayer activePid | activePid /= qpid]
                  -- Push the answer ahead of the in-flight continuation already in
                  -- the persistent queue, then drain — exactly updateGame's
                  -- (ClearUI : ... messages) <> currentQueue.
                  runGameApp app (pushAll (ClearUI : bracketed))
                  runGameApp app (runMessages "self-play" Nothing)
                  loop (steps + 1)

{- | The parked seat + its question. Prefers the active player's question (the one
the engine just set), falling back to any parked seat. 'Nothing' when nothing is
parked (a stuck, not-over state).
-}
parkedQuestion :: Game -> Maybe (PlayerId, Question Message)
parkedQuestion game = case Map.toList (gameQuestion game) of
  [] -> Nothing
  qs ->
    let active = gameActivePlayerId game
     in find ((== active) . fst) qs `orElse` listToMaybe qs
 where
  orElse (Just x) _ = Just x
  orElse Nothing y = y

{- | The 'Answer' for a parked question. Campaign-setup settings questions are not
index questions 'decideAi' can answer, so they get an empty settings answer
directly (The Gathering is the first scenario — an empty log is correct). Every
other question is answered by 'decideAi' over the parked game (read-only), the same
call the server makes for AI seats.
-}
decideAnswer :: GameApp -> PlayerId -> Question Message -> IO Answer
decideAnswer app qpid q = case unwrapQuestion q of
  PickCampaignSettings -> pure (CampaignSettingsAnswer (CampaignSettings [] mempty mempty []))
  PickScenarioSettings -> pure (StandaloneSettingsAnswer [])
  _ -> runGameApp app (decideAi rolandAiState qpid q)

-- ---------------------------------------------------------------------------
-- main

main :: IO ()
main = do
  opts <- parseArgs =<< getArgs
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  provider <- initializeGlobalTracerProvider
  let tracer = makeTracer provider $(detectInstrumentationLibrary) tracerOptions
      seeds = [optSeed opts .. optSeed opts + optGames opts - 1]

  hPutStrLn stderr $
    "arkham-self-play: "
      <> show (optGames opts)
      <> " game(s), scenario "
      <> show (optScenario opts)
      <> ", difficulty "
      <> show (optDifficulty opts)
      <> ", seeds "
      <> show (optSeed opts)
      <> ".."
      <> show (optSeed opts + optGames opts - 1)

  outcomes <- forM seeds $ \seed -> do
    outcome <- runOneGame tracer (optScenario opts) (optDifficulty opts) (optMaxSteps opts) seed
    hPutStrLn stderr $ "  seed " <> show seed <> " -> " <> show outcome
    pure outcome

  printSummary opts outcomes

printSummary :: Opts -> [Outcome] -> IO ()
printSummary opts outcomes = do
  let total = length outcomes
      count o = length (filter (== o) outcomes)
      wins = count Win
      losses = count Loss
      partials = count Partial
      aborted = count Aborted
      completed = total - aborted
      pct n d = if d == 0 then 0 else (100 * fromIntegral n / fromIntegral d :: Double)
  putStrLn "==== arkham self-play summary ===="
  putStrLn $ "scenario:   " <> show (optScenario opts)
  putStrLn $ "difficulty: " <> show (optDifficulty opts)
  putStrLn $ "games:      " <> show total
  putStrLn $ "wins:       " <> show wins
  putStrLn $ "losses:     " <> show losses
  putStrLn $ "partials:   " <> show partials
  putStrLn $ "aborted:    " <> show aborted <> " (did not reach a result)"
  putStrLn $
    "win-rate:   "
      <> showPct (pct wins total)
      <> " of all games ("
      <> show wins
      <> "/"
      <> show total
      <> ")"
  putStrLn $
    "win-rate*:  "
      <> showPct (pct wins completed)
      <> " of completed games ("
      <> show wins
      <> "/"
      <> show completed
      <> ")"

showPct :: Double -> String
showPct p =
  let scaled = round (p * 10) :: Integer
   in show (scaled `div` 10) <> "." <> show (scaled `mod` 10) <> "%"
