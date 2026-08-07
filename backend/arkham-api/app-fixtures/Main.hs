{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{- | Achievement verification fixtures (arkham-fixtures).

Builds one solo, ready-to-play game per Innsmouth Conspiracy achievement and
inserts it straight into the dev database for a given user, so each can be opened
in the browser and finished off with a single action.

== How a fixture is built (mirrors the create-game handler)

'Api.Handler.Arkham.Games.postApiV1ArkhamGamesR' inserts the game row, inserts
the player row, and only then builds the in-memory game with
@addPlayer (PlayerId \<the player row's uuid\>)@. We do the same, because the
seat's 'PlayerId' IS the @arkham_players@ row id — get that wrong and the browser
cannot find its seat.

Deck selection is the one thing that cannot happen headless: @StartCampaign@
parks on a deck prompt that needs a saved deck row. We dodge it the way
@arkham-self-play@ does, by registering the seat as an AI player
('RegisterAiPlayer'), which makes @chooseDecksWithAi@ load Roland Banks' bundled
decklist in place instead of prompting. The AI registration is wiped from
'gameSettings' before the game is written, so what lands in the database is an
ordinary human solo game.

Setup questions (campaign settings, scenario setup choices, the first mythos
phase) are answered by the AI decision engine — the same call the server makes
for AI seats. We stop as soon as the game reaches the investigation phase, so the
fixture is handed over at the start of the investigator's turn.

== Staging

Each fixture then runs 'fxStage': campaign-store writes (the achievement trackers
live there), set-aside cards moved into play, scenario meta rewrites. Anything a
fixture cannot stage legally is left to the user as a documented final action,
printed per fixture at the end of the run.
-}
module Main where

import Api.Arkham.Helpers (GameApp (..), GameAppT, runGameApp)
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (Solo))
import Arkham.Achievement.Types
import Arkham.Act.Cards qualified as Acts
import Arkham.Ai.Decision (decideAi, unwrapQuestion)
import Arkham.Ai.Decks (rolandCoreDeck)
import Arkham.Ai.State (AiPlayerState, defaultAiPlayerState)
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey (CampaignLogKey, toCampaignLogKey)
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Card (toCardCode, unCardCode)
import Arkham.Classes.HasQueue (newQueue, push, pushAll)
import Arkham.Classes.Query (selectOne)
import Arkham.Difficulty (Difficulty (..))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game (Game (..), addPlayer, newCampaign, runMessages)
import Arkham.Game.Settings (settingsAiPlayers, settingsUltimatumsAndBoons)
import Arkham.Game.State (GameState (IsOver))
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Message (createEnemyAt)
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Id
import Arkham.Matcher (InvestigatorMatcher (Anyone))
import Arkham.Message
import Arkham.Name (toTitle)
import Arkham.Phase (Phase (InvestigationPhase))
import Arkham.Placement (Placement (InPlayArea))
import Arkham.Prelude (toResultDefault)
import Arkham.Queue (queueToRef)
import Arkham.Scenario.Types (Field (ScenarioMeta))
import Arkham.Scenarios.InTooDeep.Helpers qualified as InTooDeep
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers qualified as Elina
import Arkham.SortedPair (unSortedPair)
import Arkham.Source (Source (ScenarioSource))
import Arkham.Target (Target (CampaignTarget, EnemyTarget))
import Arkham.UltimatumsAndBoons.Types (Ultimatum (..), UltimatumOrBoon (..))
import Control.Exception (SomeException, evaluate, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Random (getRandom, mkStdGen)
import Data.Aeson (ToJSON, toJSON)
import Data.Aeson.Key qualified as Key
import Data.ByteString.Char8 qualified as BS8
import Data.Coerce (coerce)
import Data.Foldable (for_, traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Postgresql (
  ConnectionPool,
  ConnectionString,
  Entity (..),
  SqlPersistT,
  deleteWhere,
  getBy,
  insert,
  insert_,
  replace,
  runSqlPool,
  withPostgresqlPool,
  (==.),
 )
import Entity.Answer (
  Answer (CampaignSettingsAnswer, StandaloneSettingsAnswer),
  CampaignSettings (CampaignSettings),
  Reply (Handled, Unhandled),
  deckChosen,
  handleAnswerPure,
 )
import Entity.Arkham.Game
import Entity.Arkham.Player
import Entity.Arkham.Step
import Entity.User (Unique (UniqueEmail))
import OpenTelemetry.Trace (
  Tracer,
  detectInstrumentationLibrary,
  initializeGlobalTracerProvider,
  makeTracer,
  tracerOptions,
 )
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import Prelude

-- ---------------------------------------------------------------------------
-- Fixed identities

theInnsmouthConspiracyId :: CampaignId
theInnsmouthConspiracyId = "07"

{- | Roland Banks — the one investigator the AI MVP ships a bundled deck for
('Arkham.Ai.Decks.bundledDeckFor' @\"01001\"@), and therefore the only seat that
can be created without a saved deck row.
-}
rolandAiState :: AiPlayerState
rolandAiState = defaultAiPlayerState "01001"

-- | Roland's investigator id, needed before his deck is loaded (see 'driveSetup').
rolandInvestigatorId :: InvestigatorId
rolandInvestigatorId = "01001"

-- Scenario ids.
thePitOfDespairId, theVanishingOfElinaHarperId, inTooDeepId :: ScenarioId
thePitOfDespairId = "07041"
theVanishingOfElinaHarperId = "07056"
inTooDeepId = "07123"

horrorInHighGearId, aLightInTheFogId, intoTheMaelstromId :: ScenarioId
horrorInHighGearId = "07198"
aLightInTheFogId = "07231"
intoTheMaelstromId = "07311"

{- | Achievement store keys, mirrored from
"Arkham.Campaign.Campaigns.TheInnsmouthConspiracy.Achievements".
-}
amalgamDefeatsKey, deepOnesDefeatedKey, lairOfDagonUndisturbedKey :: Text
amalgamDefeatsKey = "ticAchAmalgamDefeats"
deepOnesDefeatedKey = "ticAchDeepOnesDefeated"
lairOfDagonUndisturbedKey = "ticAchLairOfDagonUndisturbed"

-- ---------------------------------------------------------------------------
-- Fixture descriptions

{- | Everything that varies between fixtures. 'fxStage' runs after setup has
finished and returns extra report lines for ids the user needs (which suspect is
the right one, which raw message finishes it off).
-}
data Fixture = Fixture
  { fxAchievement :: TheInnsmouthConspiracyAchievement
  , fxLabel :: Text
  , fxScenario :: ScenarioId
  , fxDifficulty :: Difficulty
  , fxUltimatums :: [UltimatumOrBoon]
  , fxRecords :: [CampaignLogKey]
  -- ^ campaign-log records pre-seeded as if earlier scenarios had happened
  , fxInvestigatorRecords :: [CampaignLogKey]
  -- ^ the same, but recorded against each investigator
  , fxStage :: GameAppT [Text]
  , fxAction :: [Text]
  }

fixture
  :: TheInnsmouthConspiracyAchievement -> Text -> ScenarioId -> [Text] -> Fixture
fixture achievement label scenario action =
  Fixture
    { fxAchievement = achievement
    , fxLabel = label
    , fxScenario = scenario
    , fxDifficulty = Standard
    , fxUltimatums = []
    , fxRecords = []
    , fxInvestigatorRecords = []
    , fxStage = pure []
    , fxAction = action
    }

fixtures :: [Fixture]
fixtures =
  [ ( fixture
        WouldYouJustDieAlready
        "Would You Just Die Already"
        thePitOfDespairId
        [ "The Amalgam is in play at your location with 2 damage already on it (3 health)."
        , "Defeat it once - that is its 5th defeat this scenario."
        ]
    )
      { fxStage = do
          setStore amalgamDefeatsKey (4 :: Int)
          softenedAmalgam
          runMessages "fixture" Nothing
          pure []
      }
  , ( fixture
        GoneFishing
        "Gone Fishing"
        thePitOfDespairId
        [ "The Amalgam (which has the Deep One trait) is in play at your location with"
        , "2 damage on it. Defeat it - that is the 20th Deep One defeated this campaign."
        ]
    )
      { fxStage = do
          setStore deepOnesDefeatedKey (19 :: Int)
          softenedAmalgam
          runMessages "fixture" Nothing
          pure []
      }
  , ( fixture
        ElementaryDearDawson
        "Elementary, Dear Dawson"
        theVanishingOfElinaHarperId
        [ "Play to the end of a round, then use The Search for Agent Harper's objective"
        , "(\"at the end of the round\") to advance the act, and name the suspect and"
        , "hideout printed below. Naming both correctly earns it."
        ]
    )
      { fxStage = do
          kidnapper <- Elina.getKidnapper
          hideout <- Elina.getHideout
          pure
            [ "  correct suspect: " <> toTitle kidnapper
            , "  correct hideout: " <> toTitle hideout
            ]
      }
  , ( fixture
        AintNothinGonnaBreakMyStride
        "Ain't Nothin Gonna Break My Stride"
        inTooDeepId
        [ "Every barrier has been removed except one. Destroy that last one and the"
        , "achievement fires; the debug raw message that does it is printed below."
        ]
    )
      { fxStage = leaveOneBarrier
      }
  , ( fixture
        SpeedingTicket
        "Speeding Ticket"
        horrorInHighGearId
        [ "Nothing has disqualified the run yet. Reach Falcon Point Approach without"
        , "stopping a car, getting out of a car, or driving into a Long Way Around."
        ]
    )
      { fxStage =
          pure
            [ "  shortcut (debug raw message): "
                <> "{\"tag\":\"AdvanceAct\",\"contents\":[\""
                <> unCardCode (toCardCode Acts.pedalToTheMetal)
                <> "\",{\"tag\":\"TestSource\",\"contents\":[]},{\"tag\":\"AdvancedWithOther\"}]}"
            ]
      }
  , fixture
      YoureLockedInHereWithMe
      "You're Locked In Here With Me"
      aLightInTheFogId
      [ "Nobody has been captured. Finish the scenario - or send the debug raw message"
      , "{\"tag\":\"EndOfGame\",\"contents\":null} - and the achievement fires."
      ]
  , ( fixture
        FishOutOfWater
        "Fish Out of Water"
        intoTheMaelstromId
        [ "You resigned in the Moon Room, so you start Into the Maelstrom wearing a Diving"
        , "Suit. Finish the scenario - or send {\"tag\":\"EndOfGame\",\"contents\":null} -"
        , "while still wearing it."
        ]
    )
      { fxInvestigatorRecords = [toCampaignLogKey PossessesADivingSuit]
      }
  , ( fixture
        DontWakeDaddy
        "Don't Wake Daddy"
        intoTheMaelstromId
        [ "The Lair of Dagon is already recorded as finished with Dagon asleep. Finish"
        , "Into the Maelstrom - or send {\"tag\":\"EndOfGame\",\"contents\":null} - without"
        , "waking Dagon or Hydra."
        ]
    )
      { fxStage = do
          setStore lairOfDagonUndisturbedKey True
          runMessages "fixture" Nothing
          pure []
      }
  , ( fixture
        FullBuild
        "Full Build"
        aLightInTheFogId
        [ "The Waveworn Idol and the Awakened Mantle are already in your play area and the"
        , "Headdress of Y'ha-nthlei is in your hand. Play the Headdress to complete the set."
        ]
    )
      { fxRecords =
          [ toCampaignLogKey TheIdolWasBroughtToTheLighthouse
          , toCampaignLogKey TheMantleWasBroughtToTheLighthouse
          , toCampaignLogKey TheHeaddressWasBroughtToTheLighthouse
          ]
      , fxStage = stageFullBuild
      }
  , fixture
      BiggerFishToFry
      "Bigger Fish to Fry"
      intoTheMaelstromId
      [ "No Deep One has been defeated this campaign. Send the debug raw message"
      , "{\"tag\":\"CampaignStep\",\"contents\":{\"tag\":\"EpilogueStep\"}} to reach the"
      , "epilogue and win the campaign."
      ]
  , ( fixture
        InnsmouthLineInTheSand
        "Line in the Sand"
        intoTheMaelstromId
        [ "Three Ultimatums are active. Send the debug raw message"
        , "{\"tag\":\"CampaignStep\",\"contents\":{\"tag\":\"EpilogueStep\"}} to win the campaign."
        ]
    )
      { fxUltimatums =
          [ Ultimatum UltimatumOfFailure
          , Ultimatum UltimatumOfHardship
          , Ultimatum UltimatumOfDread
          ]
      }
  , ( fixture
        InnsmouthExpertise
        "Innsmouth Expertise"
        intoTheMaelstromId
        [ "The campaign is on Expert. Send the debug raw message"
        , "{\"tag\":\"CampaignStep\",\"contents\":{\"tag\":\"EpilogueStep\"}} to win the campaign."
        ]
    )
      { fxDifficulty = Expert
      }
  ]

-- ---------------------------------------------------------------------------
-- Staging helpers

-- | Push a campaign-store write, the shape the detection module reads back.
setStore :: ToJSON a => Text -> a -> GameAppT ()
setStore k v = push $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

{- | Put The Amalgam into play at the investigator's location with 2 of its 3
health already gone, so one fight finishes it.
-}
softenedAmalgam :: GameAppT ()
softenedAmalgam = do
  miid <- selectOne Anyone
  for_ miid \iid -> do
    mlid <- getLocationOf iid
    for_ mlid \lid -> do
      card <- getSetAsideCard Enemies.theAmalgam
      (eid, msg) <- createEnemyAt card lid Nothing
      pushAll [msg, PlaceDamage ScenarioSource (EnemyTarget eid) 2]

{- | Zero every barrier in In Too Deep except one, and report the raw message that
takes the last one down. In Too Deep keeps its barrier counts in the scenario
meta, so this is a meta rewrite rather than a pile of decrements.
-}
leaveOneBarrier :: GameAppT [Text]
leaveOneBarrier = do
  InTooDeep.Meta barriers <-
    scenarioFieldMap ScenarioMeta (toResultDefault $ InTooDeep.Meta mempty)
  case find ((> 0) . snd) (Map.toList barriers) of
    Nothing -> pure ["  no barriers found in the scenario meta - nothing staged"]
    Just (edge, _) -> do
      let barriers' = Map.insert edge 1 (Map.map (const 0) barriers)
          (l1, l2) = unSortedPair edge
      push $ SetScenarioMeta $ toJSON $ InTooDeep.Meta barriers'
      runMessages "fixture" Nothing
      pure
        [ "  last barrier is between " <> tshow l1 <> " and " <> tshow l2
        , "  shortcut (debug raw message): "
            <> "{\"tag\":\"ScenarioCountDecrementBy\",\"contents\":[{\"tag\":\"Barriers\","
            <> "\"contents\":[\""
            <> tshow l1
            <> "\",\""
            <> tshow l2
            <> "\"]},1]}"
        ]

{- | Two of the three Devil Reef relics into the play area, the third into hand,
so playing it completes the set. All three are set aside by A Light in the Fog's
setup because the fixture pre-records that they were brought to the lighthouse.
-}
stageFullBuild :: GameAppT [Text]
stageFullBuild = do
  miid <- selectOne Anyone
  case miid of
    Nothing -> pure ["  no investigator found - nothing staged"]
    Just iid -> do
      idol <- getSetAsideCard Assets.wavewornIdol
      mantle <- getSetAsideCard Assets.awakenedMantle
      headdress <- getSetAsideCard Assets.headdressOfYhaNthlei
      for_ [idol, mantle] \card -> do
        aid <- getRandom
        push $ CreateAssetAt aid card (InPlayArea iid)
      push $ AddToHand iid [headdress]
      runMessages "fixture" Nothing
      pure ["  play the Headdress of Y'ha-nthlei from your hand"]

-- ---------------------------------------------------------------------------
-- Building one fixture game

{- | The parked seat + its question, preferring the active player's (the one the
engine just set).
-}
parkedQuestion :: Game -> Maybe (PlayerId, Question Message)
parkedQuestion game = case Map.toList (gameQuestion game) of
  [] -> Nothing
  qs -> case find ((== gameActivePlayerId game) . fst) qs of
    Just q -> Just q
    Nothing -> listToMaybe qs

{- | Drive setup with the AI decision engine until the first investigator turn
begins, which is where a fixture is handed over.

The stop condition is @gameTurnPlayerInvestigatorId@, not the phase: the phase is
already 'InvestigationPhase' during the opening-hand mulligan, which happens
before the scenario has finished placing the board. It only becomes 'Just' at
@BeginTurn@, by which point setup is complete.

Campaign/standalone settings questions are not index questions 'decideAi' can
answer, so they are answered directly — and the settings answer is also where a
fixture's pre-seeded campaign log is injected, since it must land before scenario
setup reads it.
-}
driveSetup :: GameApp -> Fixture -> Int -> IO ()
driveSetup app fx = loop
 where
  gameRef = appGame app
  genRef = appGen app
  warn = hPutStrLn stderr
  loop 0 = warn "  ! setup did not reach the investigation phase within the step cap"
  loop stepsLeft = do
    game <- readIORef gameRef
    if gameGameState game == IsOver
      || (gamePhase game == InvestigationPhase && isJust (gameTurnPlayerInvestigatorId game))
      then pure ()
      else case parkedQuestion game of
        Nothing -> warn "  ! setup parked with no question"
        Just (qpid, q) -> do
          writeIORef genRef (mkStdGen (gameSeed game))
          let question = unwrapQuestion q
          -- Deck prompts (a mid-campaign scenario parks an upgrade prompt) are
          -- refused by handleAnswerPure, which needs a DB write to record the
          -- seat's investigator. The seat is already bound to Roland, so the
          -- messages the real handler would produce are built directly.
          (reply, isSettings) <- case question of
            ChooseUpgradeDeck -> pure (Handled (deckChosen game qpid rolandCoreDeck), False)
            ChooseDeck -> pure (Handled (deckChosen game qpid rolandCoreDeck), False)
            _ -> do
              answer <- case question of
                PickCampaignSettings ->
                  pure $ CampaignSettingsAnswer (CampaignSettings (fxRecords fx) mempty mempty [])
                PickScenarioSettings -> pure $ StandaloneSettingsAnswer []
                _ -> runGameApp app (decideAi rolandAiState qpid q)
              r <- handleAnswerPure game qpid answer
              pure (r, question == PickCampaignSettings)
          case reply of
            Unhandled why ->
              warn $ "  ! unhandled answer to " <> take 60 (show question) <> ": " <> T.unpack why
            Handled answerMessages -> do
              -- Per-investigator records have no CampaignSettings slot, so they
              -- ride along right behind the log the settings answer installs.
              -- The seat's investigator is fixed (the bundled AI deck is Roland's)
              -- and is NOT loaded yet at this point, so its id is used directly
              -- rather than read off the game.
              let extraMessages =
                    [ RecordForInvestigator rolandInvestigatorId k
                    | isSettings
                    , k <- fxInvestigatorRecords fx
                    ]
                  activePid = gameActivePlayerId game
                  bracketed =
                    [SetActivePlayer qpid | activePid /= qpid]
                      <> answerMessages
                      <> extraMessages
                      <> [SetActivePlayer activePid | activePid /= qpid]
              runGameApp app (pushAll (ClearUI : bracketed))
              runGameApp app (runMessages "fixture-setup" Nothing)
              loop (stepsLeft - 1)

-- | Build the in-memory game for a fixture: setup, staging, AI registration wiped.
buildGame :: Tracer -> Fixture -> PlayerId -> Game -> IO (Game, [Message], [Text])
buildGame tracer fx pid game0 = do
  gameRef <- newIORef game0
  queueRef <- newQueue []
  genRef <- newIORef (mkStdGen (gameSeed game0))
  let app = GameApp gameRef queueRef genRef (const (pure ())) tracer Nothing
  runGameApp app $ do
    addPlayer pid
    push (RegisterAiPlayer pid rolandAiState)
    runMessages "fixture-setup" Nothing
  driveSetup app fx 600
  extra <- runGameApp app (fxStage fx)
  game <- readIORef gameRef
  queue <- readIORef (queueToRef queueRef)
  let settings = (gameSettings game) {settingsAiPlayers = mempty}
  pure (game {gameSettings = settings}, queue, extra)

-- | Create one fixture's rows, mirroring the create-game handler's ordering.
insertFixture :: Tracer -> Text -> Fixture -> SqlPersistT IO [Text]
insertFixture tracer email fx = do
  mUser <- getBy (UniqueEmail email)
  userId <- case mUser of
    Nothing -> liftIO $ die $ "No user with email " <> T.unpack email
    Just (Entity uid _) -> pure uid
  now <- liftIO getCurrentTime
  seed <- liftIO getRandom
  let name = "[Achievement] " <> fxLabel fx
      base =
        newCampaign theInnsmouthConspiracyId (Just (fxScenario fx)) seed 1 (fxDifficulty fx) False
      settings =
        (gameSettings base) {settingsUltimatumsAndBoons = Set.fromList (fxUltimatums fx)}
      game0 = base {gameSettings = settings}
  -- Re-running replaces the previous fixture rather than piling up games.
  deleteWhere [ArkhamGameName ==. name]
  gameId <- insert $ ArkhamGame name game0 0 Solo now now
  -- The seat's PlayerId must be the arkham_players row id (see module header).
  playerId <- insert $ ArkhamPlayer userId gameId "00000"
  (game, queue, extra) <- liftIO $ buildGame tracer fx (PlayerId (coerce playerId)) game0
  replace gameId $ ArkhamGame name game 0 Solo now now
  insert_ $ ArkhamStep gameId (Choice mempty queue) 0 (ActionDiff [])
  pure
    $ [ ""
      , fxLabel fx <> "  (" <> tshow (fxAchievement fx) <> ")"
      , "  game: " <> name
      , "  url:  /arkham/games/" <> tshow (unArkhamGameKey gameId)
      ]
      <> map ("  " <>) (fxAction fx)
      <> extra

-- ---------------------------------------------------------------------------
-- main

usage :: String
usage =
  unlines
    [ "Usage: arkham-fixtures [--email ADDRESS] [--only ACHIEVEMENT ...]"
    , ""
    , "  Inserts one solo Innsmouth Conspiracy game per achievement into the database"
    , "  named by DATABASE_URL, owned by the given user, and prints how to finish each."
    , ""
    , "  --email ADDRESS  Owner of the games (default halogenandtoast@gmail.com)."
    , "  --only NAME      Only build these achievements (constructor names, repeatable)."
    ]

data Opts = Opts {optEmail :: Text, optOnly :: [Text]}

parseArgs :: [String] -> IO Opts
parseArgs = go (Opts "halogenandtoast@gmail.com" [])
 where
  go o [] = pure o
  go _ ("--help" : _) = die usage
  go _ ("-h" : _) = die usage
  go o ("--email" : v : rest) = go o {optEmail = T.pack v} rest
  go o ("--only" : v : rest) = go o {optOnly = optOnly o <> [T.pack v]} rest
  go _ (x : _) = die $ "Unexpected argument: " <> x <> "\n" <> usage

main :: IO ()
main = do
  opts <- parseArgs =<< getArgs
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  connStr <- maybe defaultConnStr BS8.pack <$> lookupEnv "DATABASE_URL"
  provider <- initializeGlobalTracerProvider
  let tracer = makeTracer provider $(detectInstrumentationLibrary) tracerOptions
      selected =
        if null (optOnly opts)
          then fixtures
          else filter ((`elem` optOnly opts) . tshow . fxAchievement) fixtures

  runNoLoggingT $ withPostgresqlPool connStr 1 \pool -> liftIO do
    -- One fixture that cannot be staged must not take the rest of the run with it.
    reports <- traverse (buildOne pool tracer (optEmail opts)) selected
    putStrLn "==== Innsmouth Conspiracy achievement fixtures ===="
    traverse_ (putStrLn . T.unpack) (concat reports)

buildOne :: ConnectionPool -> Tracer -> Text -> Fixture -> IO [Text]
buildOne pool tracer email fx =
  try @SomeException (forceReport =<< runSqlPool (insertFixture tracer email fx) pool) >>= \case
    Right report -> pure report
    Left err ->
      pure
        [ ""
        , fxLabel fx <> "  (" <> tshow (fxAchievement fx) <> ")"
        , "  FAILED: " <> T.pack (takeWhile (/= '\n') (show err))
        ]

{- | Force every report line. 'insertFixture' builds them lazily, so without this
a staging error (e.g. a scenario meta that never got written) escapes the 'try'
in 'buildOne' and only blows up when the report is printed.
-}
forceReport :: [Text] -> IO [Text]
forceReport report = traverse_ (evaluate . T.length) report >> pure report

defaultConnStr :: ConnectionString
defaultConnStr = "postgres://localhost:5432/arkham-horror-backend"

tshow :: Show a => a -> Text
tshow = T.pack . show
