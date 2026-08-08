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
import Arkham.Campaign.Types (CampaignAttrs (..), logL, metaL, stepL, storeL)
import Arkham.CampaignLog (
  CampaignLog,
  CampaignLogPartner (..),
  PartnerStatus (Resolute, Safe),
  partnersL,
  statusL,
 )
import Arkham.CampaignLogKey (CampaignLogKey, recorded, toCampaignLogKey)
import Arkham.CampaignStep (CampaignStep (CampaignSpecificStep))
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Campaigns.EdgeOfTheEarth.Partner (expeditionTeam)
import Arkham.Campaigns.EdgeOfTheEarth.Seal (Seal (..), SealKind (..))
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Campaigns.TheScarletKeys.Key qualified as TSK
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher (scarletKeyIs)
import Arkham.Campaigns.TheScarletKeys.Meta (MapLocationId (..), TheScarletKeysMeta (..), initMeta)
import Arkham.Card (Card, CardDef, lookupCard, toCardCode, toCardId, unCardCode)
import Arkham.Card.Id (unsafeMakeCardId)
import Arkham.ChaosToken.Types (ChaosTokenFace (ElderThing, FrostToken, Tablet))
import Arkham.Classes.Entity (Entity (overAttrs))
import Arkham.Classes.HasQueue (newQueue, push, pushAll)
import Arkham.Classes.Query (select, selectCount, selectOne, selectWithField)
import Arkham.Deck qualified as Deck
import Arkham.Difficulty (Difficulty (..))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyDamage, EnemyHealth, EnemyRemainingHealth))
import Arkham.Entities (Entities (..))
import Arkham.Event.Cards qualified as Events
import Arkham.Exhaust (mkExhaustion)
import Arkham.Game (Game (..), addPlayer, modeL, newCampaign, runMessages)
import Arkham.Game.Settings (settingsAiPlayers, settingsUltimatumsAndBoons)
import Arkham.Game.State (GameState (IsOver))
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Message (createEnemyAt)
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Id
import Arkham.Investigator.Types (InvestigatorAttrs (..))
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid (Pos (..))
import Arkham.Location.Types qualified as Location
import Arkham.Matcher hiding (PerformAction, PlaceUnderneath)
import Arkham.Message
import Arkham.Name (toTitle)
import Arkham.Phase (Phase (InvestigationPhase))
import Arkham.Placement (Placement (AtLocation, AttachedToInvestigator, InPlayArea))
import Arkham.Prelude (asks, at, ix, toResultDefault, (%~), (&), (.~), (?~))
import Arkham.Projection (field)
import Arkham.Queue (queueToRef)
import Arkham.Scenario.Types (Field (ScenarioMeta))
import Arkham.Scenario.Types qualified as Scenario
import Arkham.Scenarios.InTooDeep.Helpers qualified as InTooDeep
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers qualified as Elina
import Arkham.SortedPair (unSortedPair)
import Arkham.Source
import Arkham.Story.Cards qualified as Stories
import Arkham.Target
import Arkham.Token qualified as Token
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.UltimatumsAndBoons.Types (Ultimatum (..), UltimatumOrBoon (..))
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (replicateM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Random (getRandom, mkStdGen)
import Data.Aeson (ToJSON, encode, toJSON)
import Data.Aeson.Key qualified as Key
import Data.Bifunctor (first, second)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce (coerce)
import Data.Foldable (for_, toList, traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
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
  Answer (StandaloneSettingsAnswer),
  CampaignSettings (CampaignSettings),
  Reply (Handled, Unhandled),
  deckChosen,
  handleAnswerPure,
  makeCampaignLog,
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

theInnsmouthConspiracyId, edgeOfTheEarthId, theScarletKeysId :: CampaignId
theInnsmouthConspiracyId = "07"
edgeOfTheEarthId = "08"
theScarletKeysId = "09"

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
  { fxAchievement :: Achievement
  , fxLabel :: Text
  , fxCampaign :: CampaignId
  , fxScenario :: Maybe ScenarioId
  {- ^ Nothing for a fixture that lives on a CAMPAIGN step (e.g. the world map)
  rather than inside a scenario; 'fxCampaignSeed' then sets the step.
  -}
  , fxDifficulty :: Difficulty
  , fxUltimatums :: [UltimatumOrBoon]
  , fxRecords :: [CampaignLogKey]
  -- ^ campaign-log records pre-seeded as if earlier scenarios had happened
  , fxCounts :: Map.Map CampaignLogKey Int
  -- ^ campaign-log record COUNTS, seeded the same way (e.g. Scarlet Keys' Time)
  , fxInvestigatorRecords :: [CampaignLogKey]
  -- ^ the same, but recorded against each investigator
  , fxStage :: GameAppT [Text]
  , fxCampaignEdit :: CampaignAttrs -> CampaignAttrs
  {- ^ applied to the finished campaign; for state no message can set, such as
  the campaign-level chaos bag the epilogue reads
  -}
  , fxStopAtLabel :: Maybe Text
  {- ^ stop driving setup as soon as the parked question's encoded form mentions
  this label, leaving the game sitting ON that prompt. For fixtures whose
  achievement fires from a setup CHOICE, which the AI would otherwise answer.
  -}
  , fxCampaignSeed :: CampaignAttrs -> CampaignAttrs
  {- ^ applied BEFORE setup runs, for state the scenario's own setup reads back.
  Jumping straight to a mid-campaign scenario skips the prologue, so a campaign
  whose meta is initialised there (The Scarlet Keys) has to be seeded here.
  -}
  , fxAction :: [Text]
  }

fixture
  :: TheInnsmouthConspiracyAchievement -> Text -> ScenarioId -> [Text] -> Fixture
fixture achievement label scenario action =
  Fixture
    { fxAchievement = TheInnsmouthConspiracyAchievement achievement
    , fxLabel = label
    , fxCampaign = theInnsmouthConspiracyId
    , fxScenario = Just scenario
    , fxDifficulty = Standard
    , fxUltimatums = []
    , fxRecords = []
    , fxCounts = mempty
    , fxInvestigatorRecords = []
    , fxStage = pure []
    , fxCampaignEdit = id
    , fxStopAtLabel = Nothing
    , fxCampaignSeed = id
    , fxAction = action
    }

-- | An Edge of the Earth fixture; same shape, different campaign.
eoteFixture :: EdgeOfTheEarthAchievement -> Text -> ScenarioId -> [Text] -> Fixture
eoteFixture achievement label scenario action =
  Fixture
    { fxAchievement = EdgeOfTheEarthAchievement achievement
    , fxLabel = label
    , fxCampaign = edgeOfTheEarthId
    , fxScenario = Just scenario
    , fxDifficulty = Standard
    , fxUltimatums = []
    , fxRecords = []
    , fxCounts = mempty
    , fxInvestigatorRecords = []
    , fxStage = pure []
    , fxCampaignEdit = id
    , fxStopAtLabel = Nothing
    , fxCampaignSeed = id
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
-- Edge of the Earth

-- Scenario ids.
iceAndDeathPart1Id, fatalMirageId, toTheForbiddenPeaksId :: ScenarioId
iceAndDeathPart1Id = "08501a"
fatalMirageId = "08549"
toTheForbiddenPeaksId = "08596"

cityOfTheElderThingsId, heartOfMadnessPart1Id, heartOfMadnessPart2Id :: ScenarioId
cityOfTheElderThingsId = "08621"
heartOfMadnessPart1Id = "08648a"
heartOfMadnessPart2Id = "08648b"

{- | Achievement store keys, mirrored from
"Arkham.Campaign.Campaigns.EdgeOfTheEarth.Achievements".
-}
eoteTekeliliDrawnKey
  , eoteDyerHealedDanforthKey
  , eotePylonsCollapsedKey
  , eoteKeysCollectedKey
  , eoteKeysSpentKey
  , eoteKeysHeldKey
  , eotePartnersBroughtKey
  , eoteScenariosWithPartnerKey
  , eoteScenariosPlayedKey
    :: Text
eoteTekeliliDrawnKey = "eoteAchTekeliliDrawn"
eoteDyerHealedDanforthKey = "eoteAchDyerHealedDanforth"
eotePylonsCollapsedKey = "eoteAchPylonsCollapsed"
eoteKeysCollectedKey = "eoteAchKeysCollected"
eoteKeysSpentKey = "eoteAchKeysSpent"
eoteKeysHeldKey = "eoteAchKeysHeld"
eotePartnersBroughtKey = "eoteAchPartnersBrought"
eoteScenariosWithPartnerKey = "eoteAchScenariosWithPartner"
eoteScenariosPlayedKey = "eoteAchScenariosPlayed"

-- | The debug raw message that reaches the epilogue, i.e. wins the campaign.
epilogueShortcut :: Text
epilogueShortcut =
  "  finish with: {\"tag\":\"CampaignStep\",\"contents\":{\"tag\":\"EpilogueStep\"}}"

edgeOfTheEarthFixtures :: [Fixture]
edgeOfTheEarthFixtures =
  [ eoteFixture
      SafeBet
      "Safe Bet"
      iceAndDeathPart1Id
      [ "Crystalline Cavern is the only camp with a shelter value of 8. Camping is"
      , "recording its key, which the scenario does at its resolution."
      , "  finish with: {\"tag\":\"Record\",\"contents\":"
          <> "{\"tag\":\"EdgeOfTheEarthKey\",\"contents\":\"Camp_CrystallineCavern\"}}"
      ]
  , ( eoteFixture
        LookAtAllThisStuff
        "Look at All This Stuff!"
        toTheForbiddenPeaksId
        [ "You are carrying all seven supplies and are standing one step short of"
        , "The Summit. Move onto The Summit and it fires on arrival."
        ]
    )
      { fxStage = stageLookAtAllThisStuff
      }
  , ( eoteFixture
        InYourHead
        "In Your Head"
        fatalMirageId
        [ "Nine memory story cards are already in the victory display. Finish the"
        , "scenario - advance Shadow of the Past - and it fires."
        ]
    )
      { fxStage = stageInYourHead
      }
  , ( eoteFixture
        ChaosChaos
        "Chaos Chaos"
        cityOfTheElderThingsId
        [ "Ten keys collected and nine spent. Spend one more - place a key you are"
        , "holding back onto a location - and it fires."
        ]
    )
      { fxStage = do
          miid <- selectOne Anyone
          for_ miid \iid -> push $ PlaceKey (toTarget iid) RedKey
          runMessages "fixture" Nothing
          -- Seeded after the placement so its own cascade cannot overwrite these.
          setStore eoteKeysCollectedKey (10 :: Int)
          setStore eoteKeysSpentKey (9 :: Int)
          setStore eoteKeysHeldKey [RedKey]
          runMessages "fixture" Nothing
          pure ["  you are holding the red key; place it on a location to spend it"]
      }
  , ( eoteFixture
        KnockKnock
        "Knock, Knock"
        heartOfMadnessPart1Id
        [ "Four of the five seals are placed and active on a location. Place the"
        , "fifth and it fires on placement."
        ]
    )
      { fxStage = placeEverySeal
      }
  , ( eoteFixture
        MadWithPower
        "Mad With Power"
        heartOfMadnessPart2Id
        [ "Fifteen copies of The Nameless Madness are in play and fourteen are exhausted."
        , "Exhaust the last one and it fires."
        ]
    )
      { fxStage = stageNamelessMadness
      }
  , ( eoteFixture
        ConstructAdditionalPylons
        "Construct Additional Pylons"
        heartOfMadnessPart2Id
        [ "All five Mist-Pylons are already recorded as collapsed. Escape alive:"
        , "  finish with: {\"tag\":\"EndOfGame\",\"contents\":null}"
        ]
    )
      { fxStage = do
          setStore eotePylonsCollapsedKey True
          runMessages "fixture" Nothing
          pure []
      }
  , ( eoteFixture
        TheSoundOfMadness
        "The Sound of Madness"
        heartOfMadnessPart2Id
        [ "Nine Tekeli-li! drawn this game and a tenth sitting on top of your deck."
        , "Draw a card and it fires."
        ]
    )
      { fxStage = withInvestigator \iid -> do
          tekelili <- genPlayerCardDef Treacheries.tekelili_223
          push $ PutCardOnTopOfDeck iid (Deck.InvestigatorDeck iid) tekelili
          runMessages "fixture" Nothing
          setStore eoteTekeliliDrawnKey (9 :: Int)
          runMessages "fixture" Nothing
          pure ["  the top card of your deck is a Tekeli-li!"]
      }
  , ( eoteFixture
        SorryImAllOutOfDogPuns
        "Sorry, I'm All Out of Dog Puns"
        heartOfMadnessPart2Id
        [ "Four Sled Dogs are in your play area and Anyu is in your hand."
        , "Play Anyu and it fires."
        ]
    )
      { fxStage = stageDogPuns
      }
  , ( eoteFixture
        KindOfAHatOnAHat
        "Kind of a Hat on a Hat"
        heartOfMadnessPart2Id
        [ "A Backpack is in play with a Wooden Sledge underneath it, and a second"
        , "Backpack is on top of your deck. Play the Sledge out of the Backpack, then -"
        , "as your very next action - use the Sledge's ability and place the Backpack"
        , "underneath it. Taking any other action in between breaks the chain."
        ]
    )
      { fxStage = stageHatOnAHat
      }
  , ( eoteFixture
        ThisWasYourIdea
        "This Was Your Idea"
        heartOfMadnessPart2Id
        [ "Dyer is in play with a secret, Danforth is in play with 4 horror, and 2 horror"
        , "have already been healed off him this scenario. Use Dyer's ability on Danforth"
        , "once more."
        ]
    )
      { fxStage = stageThisWasYourIdea
      }
  , ( eoteFixture
        NoRespectForTheDead
        "No Respect For the Dead"
        heartOfMadnessPart2Id
        [ "Four Memorials of the Lost assets are in your play area and a fifth is in your"
        , "hand. Play it and it fires."
        ]
    )
      { fxStage = stageMemorials
      }
  , ( eoteFixture
        WukWukBoom
        "Wuk Wuk Boom"
        heartOfMadnessPart2Id
        [ "You control Dynamite with a supply left, and two Giant Albino Penguins share"
        , "your location. Use the Dynamite on them - 3 damage kills both."
        ]
    )
      { fxStage = stageWukWukBoom
      }
  , ( eoteFixture
        TheColdNeverBotheredMeAnyway
        "The Cold Never Bothered Me Anyway"
        heartOfMadnessPart2Id
        [ "The campaign chaos bag holds eight Frost tokens. Win the campaign:"
        , epilogueShortcut
        ]
    )
      { fxCampaignEdit = withFrostTokens 8
      }
  , ( eoteFixture
        HellFrozeOver
        "Hell Froze Over"
        heartOfMadnessPart2Id
        [ "The campaign chaos bag holds no Frost tokens at all. Win the campaign:"
        , epilogueShortcut
        ]
    )
      { fxCampaignEdit = withFrostTokens 0
      }
  , eoteFixture
      AbandonedAndAlone
      "Abandoned and Alone"
      heartOfMadnessPart2Id
      [ "No partner has ever been taken into a scenario. Win the campaign:"
      , epilogueShortcut
      ]
  , ( eoteFixture
        FriendsForever
        "Friends Forever"
        heartOfMadnessPart2Id
        [ "Dr. Mala Sinha is in play, came along in every scenario played, and is"
        , "Resolute (she confronted her demons). Win the campaign:"
        , epilogueShortcut
        ]
    )
      { fxStage = stageFriendsForever
      , fxCampaignEdit = withResolutePartner
      }
  , ( eoteFixture
        ThereAndBackAgain
        "There and Back Again"
        heartOfMadnessPart2Id
        [ "All nine expedition members are recorded as survivors. Win the campaign:"
        , epilogueShortcut
        ]
    )
      { fxStage = do
          push
            $ RecordSetInsert (toCampaignLogKey TheSurvivorsOfTheExpeditionWere)
            $ map (recorded . toCardCode) (toList expeditionTeam)
          runMessages "fixture" Nothing
          pure []
      }
  , ( eoteFixture
        SnowLineInTheSand
        "Line in the...Snow"
        heartOfMadnessPart2Id
        [ "Three Ultimatums are active. Win the campaign:"
        , epilogueShortcut
        ]
    )
      { fxUltimatums =
          [ Ultimatum UltimatumOfFailure
          , Ultimatum UltimatumOfHardship
          , Ultimatum UltimatumOfDread
          ]
      }
  , ( eoteFixture
        AntarcticExpertise
        "Antarctic Expertise"
        heartOfMadnessPart2Id
        [ "The campaign is on Expert. Win the campaign:"
        , epilogueShortcut
        ]
    )
      { fxDifficulty = Expert
      }
  ]

-- ---------------------------------------------------------------------------
-- The Scarlet Keys

-- | Congress of the Keys, the campaign's finale.
deadHeatId, dealingsInTheDarkId, onThinIceId, dogsOfWarId, shadesOfSufferingId :: ScenarioId
deadHeatId = "09520"

sanguineShadowsId :: ScenarioId
sanguineShadowsId = "09545"
dealingsInTheDarkId = "09566"
onThinIceId = "09609"
dogsOfWarId = "09635"
shadesOfSufferingId = "09660"

{- | Achievement store keys, mirrored from
"Arkham.Campaign.Campaigns.TheScarletKeys.Achievements".
-}
chimeraFormsKey :: Text
chimeraFormsKey = "tskAchChimeraForms"

congressOfTheKeysId, dancingMadId :: ScenarioId
congressOfTheKeysId = "09694"
dancingMadId = "09591"

{- | Achievement store key, mirrored from
"Arkham.Campaign.Campaigns.TheScarletKeys.Achievements".
-}
taylorTalksKey, shiftedKeysKey, cuisineKey :: Text
taylorTalksKey = "tskAchTaylorTalks"
shiftedKeysKey = "tskAchShiftedKeys"
cuisineKey = "tskAchCuisine"

-- | A Scarlet Keys fixture; same shape, different campaign.
tskFixture :: TheScarletKeysAchievement -> Text -> ScenarioId -> [Text] -> Fixture
tskFixture achievement label scenario action =
  Fixture
    { fxAchievement = TheScarletKeysAchievement achievement
    , fxLabel = label
    , fxCampaign = theScarletKeysId
    , fxScenario = Just scenario
    , fxDifficulty = Standard
    , fxUltimatums = []
    , fxRecords = []
    , fxCounts = mempty
    , fxInvestigatorRecords = []
    , fxStage = pure []
    , fxCampaignEdit = id
    , fxStopAtLabel = Nothing
    , fxCampaignSeed = id
    , fxAction = action
    }

{- | Every "win the campaign" achievement in the printed list is satisfied by the
same end state, so they share ONE game rather than six near-identical ones. The
fixture is keyed on Global Expertise for @--only@; the report lists all six.
-}
theScarletKeysFixtures :: [Fixture]
theScarletKeysFixtures =
  [ ( tskFixture
        GlobalExpertise
        "Scarlet Keys - win the campaign"
        congressOfTheKeysId
        [ "One game that satisfies all six of this batch at once:"
        , "  Speed Demon            - 15 time passed (needs 17 or fewer)"
        , "  Trust Nobody           - 4 Elder Thing tokens, none ever removed"
        , "  Trust Everybody        - 4 Tablet tokens, none ever removed"
        , "  Here is Your Badge     - Foundation trust 2 vs deception 0, so the"
        , "                           epilogue records a permanent position"
        , "  Line in the Sand       - three Ultimatums active"
        , "  Global Expertise       - the campaign is on Expert"
        , ""
        , "Win Congress of the Keys (Resolution 1) - or send the debug raw message"
        , "below to jump straight to the epilogue, which is what every one of these"
        , "keys on."
        , epilogueShortcut
        ]
    )
      { fxDifficulty = Expert
      , fxUltimatums =
          [ Ultimatum UltimatumOfFailure
          , Ultimatum UltimatumOfHardship
          , Ultimatum UltimatumOfDread
          ]
      , -- Trust with no deception: the epilogue awards the permanent position when
        -- trust >= deception, and joining the Red Coterie would bypass it entirely.
        fxRecords =
          [ toCampaignLogKey TSK.TheCellToldTheTruthToTaylor
          , toCampaignLogKey TSK.AgentQuinnHasYourBack
          ]
      , fxCounts = Map.singleton (toCampaignLogKey TSK.Time) 15
      , {- The prologue is where the campaign builds its world map meta, and jumping
        to a mid-campaign scenario skips it; seed the bag here too so the scenario's
        own chaos bag is drawn from it.
        -}
        fxCampaignSeed = withTrustTokens . withScarletKeysMeta
      , fxCampaignEdit = withTrustTokens
      }
  , ( tskFixture
        ImJustHereForTheLocalCuisine
        "Scarlet Keys - Local Cuisine"
        dancingMadId
        [ "Four of the five cuisine moments are already done: Marrakesh (Dead Heat's"
        , "rooftop cafe), Buenos Aires (Sanguine Shadows' intro), Tokyo (Special"
        , "Delivery 1) and Kuala Lumpur (the Selangor Club's parley)."
        , "Havana is the one left: move back onto Cafe Luna and it fires on entry."
        ]
    )
      { fxCampaignSeed = withScarletKeysMeta
      , fxStage = stageLastMeal
      }
  , ( embarkFixture
        GiftOfGab
        "Scarlet Keys - Gift of Gab"
        (onTheMap Tokyo [] [Lagos, Rome])
        [ "Commissioner Taylor has already ordered you to \"talk\" twice. Embark to"
        , "Lagos: with no intel currently being delivered, Special Delivery runs its"
        , "hand-OVER branch, which is the third \"talk\"."
        ]
    )
      { fxStage = do
          setStore taylorTalksKey (2 :: Int)
          runMessages "fixture" Nothing
          pure []
      }
  , ( tskFixture
        ScarletWithYourPowersCombined
        "Scarlet Keys - With Your Powers Combined"
        congressOfTheKeysId
        [ "You control The Eye of Ravens and four keys are already tallied as shifted"
        , "THIS TURN, so shifting it is the fifth. Use the key's own fast ability."
        , "NOTE 1: on its STABLE side that ability is restricted to during a skill"
        , "test at your location, so start one (e.g. investigate) and then use it."
        , "NOTE 2: the tally is per TURN and is cleared at BeginTurn/EndTurn, so"
        , "shift before ending the turn you are handed."
        ]
    )
      { fxCampaignSeed = withScarletKeysMeta
      , fxStage = stageFifthShift
      }
  , ( embarkFixture
        AllHollow
        "Scarlet Keys - All Hollow"
        (onTheMap Marrakesh [] [Rome, Marrakesh])
        [ "The cell is off mission, so travelling to Rome runs Romulus and Remus,"
        , "which is what UNLOCKS the Bermuda Triangle - and that unlock is the earn."
        , "Embark to Rome."
        ]
    )
      { fxRecords = [toCampaignLogKey TSK.TheCellIsOffMission]
      }
  , ( tskFixture
        TakeThatGhulat
        "Scarlet Keys - Take That, Ghulat"
        deadHeatId
        [ "No civilian has been slain. Finish Dead Heat with the count still at zero"
        , "- or send {\"tag\":\"EndOfGame\",\"contents\":null} - and it fires."
        ]
    )
      { fxCampaignSeed = withScarletKeysMeta
      }
  , ( tskFixture
        WhatsInAName
        "Scarlet Keys - What's in a Name?"
        deadHeatId
        [ "Telling Amaranth her real name is Dead Heat's Resolution 3, which records"
        , "*Amaranth has left the Coterie*. Reach that resolution - or send the debug"
        , "raw message:"
        , "  {\"tag\":\"ScenarioResolution\",\"contents\":{\"tag\":\"Resolution\",\"contents\":3}}"
        ]
    )
      { fxCampaignSeed = withScarletKeysMeta
      }
  , ( tskFixture
        MoreLikeDestroyedChimera
        "Scarlet Keys - More Like \"Destroyed\" Chimera"
        onThinIceId
        [ "Four of the Void Chimera's five forms are already recorded as defeated."
        , "Defeat the fifth (the True Form) and it fires."
        ]
    )
      { fxCampaignSeed = withScarletKeysMeta
      , fxStage = do
          -- Four forms banked; the True Form is the one left to kill.
          setStore
            chimeraFormsKey
            (["09627", "09628", "09629", "09630"] :: [Text])
          runMessages "fixture" Nothing
          pure []
      }
  , ( tskFixture
        WhoWatchesTheWatcher
        "Scarlet Keys - Who Watches the Watcher?"
        sanguineShadowsId
        [ "The secret final act is Seeing Red, which is printed as both an agenda and"
        , "an act. It reaches the table when In the Searchlight's last step installs"
        , "it as the current agenda deck - play to that, or send:"
        , "  {\"tag\":\"SetCurrentAgendaDeck\",\"contents\":[1,[{\"tag\":\"EncounterCard\","
        , "   \"contents\":{\"cardCode\":\"c09562\"}}]]}"
        ]
    )
      { fxCampaignSeed = withScarletKeysMeta
      }
  , ( tskFixture
        UnderMyUmbrella
        "Scarlet Keys - Under My Umbrella"
        shadesOfSufferingId
        [ "Tzu San Niang has not devoured a Geist. She only can if she is still in"
        , "the shadows when agenda 2 (Restless Dead) advances - expose her first."
        , "Finish the scenario - or send {\"tag\":\"EndOfGame\",\"contents\":null}."
        ]
    )
      { fxCampaignSeed = withScarletKeysMeta
      }
  , ( tskFixture
        LostAndFound
        "Scarlet Keys - Lost and Found"
        dealingsInTheDarkId
        [ "Clues Unveiled has no clues on it (Time is low, so setup seeds none)."
        , "Take control of the Twisted Antiprism - Search for the Talisman hands it"
        , "over - and it fires while the story is still bare."
        ]
    )
      { fxCampaignSeed = withScarletKeysMeta
      }
  , ( tskFixture
        ILikeTowerDefenseGames
        "Scarlet Keys - I Like Tower Defense Games"
        dogsOfWarId
        [ "Dogs of War v. I, with every Key Locus still standing. Advance Rabbits who"
        , "Run (v. I) without one being destroyed - or send:"
        , "  {\"tag\":\"AdvanceAct\",\"contents\":[\"c09639\",{\"tag\":\"TestSource\","
        , "   \"contents\":[]},{\"tag\":\"AdvancedWithOther\"}]}"
        ]
    )
      { fxCampaignSeed = withScarletKeysMeta
      }
  , ( tskFixture
        PorqueNoLosDos
        "Scarlet Keys - Porque No Los Dos?"
        dancingMadId
        [ "Both copies of Desi are in play at your location, each softened to within"
        , "one Dynamite Blast, and the Blast is in your hand."
        , "Play it and choose your own location: 3 damage kills both at once."
        , "NOTE: Desi turns damage into an automatic evade while act 1 or 2 is out,"
        , "so the act deck has been advanced to act 3 (False Light) for you."
        ]
    )
      { fxCampaignSeed = withScarletKeysMeta
      , fxStage = stageBothDesis
      }
  , ( tskFixture
        PlayWithYourFood
        "Scarlet Keys - Play With Your Food"
        dogsOfWarId
        [ "Dogs of War v. III. The Beast in a Cowl of Crimson bears The Light of"
        , "Pharos and is down to exactly 1 remaining health, and the key already has"
        , "2 of the 3 resources it needs."
        , "At the Catacombs of Kom El Shoqafa, use its ability: pass the test, pay"
        , "the clue, and the third resource hands you the key - stealing it while"
        , "the Beast is on 1 health is the earn."
        ]
    )
      { fxCounts = Map.singleton (toCampaignLogKey TSK.Time) 20
      , fxCampaignSeed = withScarletKeysMeta
      , fxStage = stageWoundedBearer
      }
  , ( tskFixture
        BloodyRedRevolution
        "Scarlet Keys - Bloody Red Revolution"
        congressOfTheKeysId
        [ "The game is parked ON the trial's verdict prompt in Congress of the Keys'"
        , "intro. The vote came out 7 nay to 1 yea, so the Coterie-siding options are"
        , "open. Pick \"overthrow\" and it fires immediately."
        ]
    )
      { fxRecords = theTrialGoesYourWay
      , fxCampaignSeed = withGoodDesi . withScarletKeysMeta
      , fxStopAtLabel = Just "overthrow"
      }
  , ( tskFixture
        RedLooksGoodOnMe
        "Scarlet Keys - Red Looks Good on Me"
        congressOfTheKeysId
        [ "Same trial verdict prompt as Bloody Red Revolution, from the same 7-1 vote."
        , "Pick \"join\" and it fires immediately."
        ]
    )
      { fxRecords = theTrialGoesYourWay
      , fxCampaignSeed = withGoodDesi . withScarletKeysMeta
      , fxStopAtLabel = Just "join"
      }
  ]

{- | A Scarlet Keys fixture that sits on the world map rather than inside a
scenario: no scenario id, the campaign step set to @embark@, and setup driven only
as far as the embark question itself.
-}
embarkFixture
  :: TheScarletKeysAchievement -> Text -> (TheScarletKeysMeta -> TheScarletKeysMeta) -> [Text] -> Fixture
embarkFixture achievement label editMeta action =
  (tskFixture achievement label "unused" action)
    { fxScenario = Nothing
    , fxStopAtLabel = Just "embark"
    , fxCampaignSeed = \attrs ->
        attrs
          & (stepL .~ CampaignSpecificStep "embark" (Just (tshow (currentLocation (editMeta initMeta)))))
          & (metaL .~ toJSON (editMeta initMeta))
    }

{- | Put the party on the map at @from@, having already visited @visited@, with
@unlocked@ reachable. The world map is otherwise the one the prologue builds.
-}
onTheMap
  :: MapLocationId -> [MapLocationId] -> [MapLocationId] -> TheScarletKeysMeta -> TheScarletKeysMeta
onTheMap from visited unlocked meta =
  meta
    { currentLocation = from
    , visitedLocations = from : visited
    , unlockedLocations = unlocked
    }

{- | The campaign-log state that makes Congress of the Keys' trial go the cell's
way AND leaves both Coterie-siding options open.

The trial tallies a nay/yea vote per Coterie member from earlier scenarios' records
and only offers "overthrow"/"join" when nay beats yea. With these records the tally
is 7 nay to 1 yea, and the scenario's own gates both hold:

  * @canOverthrow@ = La Chica Roja voted nay (The Sanguine Watcher's Torment
    Continues) && Ece voted nay (Ece Does Not Trust the Cell is NOT recorded) &&
    Desi is the good version (the @desidarioVersion@ store key below).
  * @canJoin@ = The Claret Knight voted nay (The Cell Aided the Knight) && Tuwile
    Masai Is on Your Side && The Cell Made a Deal With Thorne.

The Cell Knows the True Nature of the Coterie is deliberately absent: it short-
circuits the trial to a different branch entirely, as does three or more Coterie
members going "eerily silent".
-}
theTrialGoesYourWay :: [CampaignLogKey]
theTrialGoesYourWay =
  map
    toCampaignLogKey
    [ TSK.TheCellAidedTheKnight
    , TSK.AmaranthHasLeftTheCoterie
    , TSK.TheCellMadeADealWithThorne
    , TSK.AlikiIsOnYourSide
    , TSK.TheSanguineWatchersTormentContinues
    , TSK.TzuSanNiangIsUnderYourSway
    , TSK.TuwileMasaiIsOnYourSide
    ]

{- | Desi's "good" version, the one that votes nay. Dancing Mad records which copy
was faced in the campaign store rather than the log, so it is seeded the same way
the scenario writes it (@CampaignSpecific "desidarioVersion"@).
-}
withGoodDesi :: CampaignAttrs -> CampaignAttrs
withGoodDesi attrs = attrs & storeL . at "desidarioVersion" ?~ toJSON ("09607" :: Text)

{- | Four of each trust token, the most 'swapTokens' will ever put in the bag,
which is what both Trust achievements ask for.
-}
withTrustTokens :: CampaignAttrs -> CampaignAttrs
withTrustTokens attrs =
  attrs
    { campaignChaosBag =
        replicate 4 ElderThing
          <> replicate 4 Tablet
          <> filter (`notElem` [ElderThing, Tablet]) (campaignChaosBag attrs)
    }

{- | The world-map meta The Scarlet Keys initialises at its prologue step. Every
campaign step past the prologue reads it back, so a fixture that starts at a
later scenario has to put it there itself.
-}
withScarletKeysMeta :: CampaignAttrs -> CampaignAttrs
withScarletKeysMeta attrs = attrs & metaL .~ toJSON initMeta

{- | Everything --only can reach. A bare run builds The Scarlet Keys alone; the
earlier campaigns' games are already made and re-running them would only churn
them.
-}
allFixtures :: [Fixture]
allFixtures = fixtures <> edgeOfTheEarthFixtures <> theScarletKeysFixtures

-- | What a bare run builds.
defaultFixtures :: [Fixture]
defaultFixtures = theScarletKeysFixtures

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
-- Edge of the Earth staging helpers

-- | The seven Ice and Death supplies, as the assets they become on the mountain.
eoteSupplyAssets :: [CardDef]
eoteSupplyAssets =
  [ Assets.greenSoapstoneJinxedIdol
  , Assets.woodenSledge
  , Assets.dynamite
  , Assets.miasmicCrystalStrangeEvidence
  , Assets.mineralSpecimen
  , Assets.smallRadio
  , Assets.spareParts
  ]

{- | All seven supplies in hand, standing on the last mountainside before The
Summit, so a single move earns it.
-}
stageLookAtAllThisStuff :: GameAppT [Text]
stageLookAtAllThisStuff = withInvestigator \iid -> do
  for_ eoteSupplyAssets $ void . putIntoPlay iid
  summit <- selectOne (locationIs Locations.theSummit)
  climbable <- selectWithField Location.LocationPosition Anywhere
  -- The mountainsides are laid out along one row with The Summit last, so the
  -- highest column that is not the summit is the step below it.
  let below =
        [ (lid, col)
        | (lid, Just (Pos _ col)) <- climbable
        , Just lid /= summit
        ]
  case sortOn (negate . snd) below of
    [] -> pure ["  no mountainside found - move up to The Summit yourself"]
    ((lid, _) : _) -> do
      push $ PlaceInvestigator iid (AtLocation lid)
      runMessages "fixture" Nothing
      pure ["  move one step to The Summit"]

-- | Fatal Mirage's memories, the Story cards its victory display is counted in.
eoteMemories :: [CardDef]
eoteMemories =
  [ Stories.memoryOfAHuntGoneAwry
  , Stories.memoryOfALostPatient
  , Stories.memoryOfAMissingFather
  , Stories.memoryOfARavagedCountry
  , Stories.memoryOfARegretfulVoyage
  , Stories.memoryOfAnUnspeakableEvil
  , Stories.memoryOfATerribleDiscovery
  , Stories.memoryOfAnAlienTranslation
  , Stories.memoryOfAnUnrequitedLove
  ]

{- | Put a set-aside card straight into the victory display. Used to represent
supplies carried to the summit without replaying the climb.
-}
addSetAsideToVictory :: CardDef -> GameAppT ()
addSetAsideToVictory def = do
  cards <- select $ SetAsideCardMatch $ cardIs def
  for_ (listToMaybe cards) \card -> push $ AddToVictory Nothing (CardIdTarget $ toCardId card)

{- | Eight memories already banished. They are not set aside in Fatal Mirage, so
the victory display is seeded directly rather than through AddToVictory.
-}
stageInYourHead :: GameAppT [Text]
stageInYourHead = do
  cards <- traverse genPlayerCardDef eoteMemories
  ref <- asks appGame
  let seed attrs = attrs {Scenario.scenarioVictoryDisplay = cards}
  liftIO $ modifyIORef' ref $ modeL %~ second (overAttrs seed)
  act <- selectOne AnyAct
  pure
    [ "  nine memories are banished; finish the scenario with:"
    , "    {\"tag\":\"AdvanceAct\",\"contents\":[\""
        <> maybe "<act>" (unCardCode . unActId) act
        <> "\",{\"tag\":\"TestSource\",\"contents\":[]},{\"tag\":\"AdvancedWithOther\"}]}"
    ]

-- | Place all five seals, active, on whatever locations are in play.
placeEverySeal :: GameAppT [Text]
placeEverySeal = do
  locations <- select Anywhere
  case listToMaybe locations of
    Nothing -> pure ["  no locations in play - nothing staged"]
    Just lid -> do
      for_ (filter (/= SealE) [minBound @SealKind ..]) \kind ->
        push $ PlaceSeal (toTarget lid) (Seal kind True Nothing)
      runMessages "fixture" Nothing
      pure
        [ "  four seals are active on one location; place the fifth with:"
        , "    {\"tag\":\"PlaceSeal\",\"contents\":[{\"tag\":\"LocationTarget\",\"contents\":\""
            <> tshow lid
            <> "\"},{\"sealKind\":\"SealE\",\"sealActive\":true}]}"
        ]

-- | Fifteen Nameless Madness in play, all but one exhausted.
stageNamelessMadness :: GameAppT [Text]
stageNamelessMadness = do
  miid <- selectOne Anyone
  mlid <- maybe (pure Nothing) getLocationOf miid
  case mlid of
    Nothing -> pure ["  no location for the investigator - nothing staged"]
    Just lid -> do
      card <- getSetAsideCardMaybe Enemies.theNamelessMadness
      for_ card \c -> replicateM_ 15 do
        (_, createMsg) <- createEnemyAt c lid Nothing
        push createMsg
      runMessages "fixture" Nothing
      madnesses <- select $ enemyIs Enemies.theNamelessMadness
      for_ (drop 1 madnesses) $ push . Exhaust . mkExhaustion ScenarioSource
      runMessages "fixture" Nothing
      pure ["  exhaust the last ready copy to finish it"]

{- | Four Sled Dogs in play and Anyu in hand. Anyu is created last, from hand,
because putting her straight into play produced nothing - she is a story asset,
and the direct-creation path does not take.
-}
stageDogPuns :: GameAppT [Text]
stageDogPuns = withInvestigator \iid -> do
  replicateM_ 4 $ void $ putIntoPlay iid Assets.sledDog
  addToHandFromNowhere iid Assets.anyuFaithfulCompanion
  runMessages "fixture" Nothing
  dogs <- selectCount $ assetControlledBy iid <> assetIs Assets.sledDog
  pure ["  play Anyu from your hand (" <> tshow dogs <> " Sled Dogs already in play)"]

{- | A Backpack in play holding a Wooden Sledge, and a second Backpack on top of
the deck so the Sledge's search can find it.
-}
stageHatOnAHat :: GameAppT [Text]
stageHatOnAHat = withInvestigator \iid -> do
  aid <- getRandom
  pack <- genPlayerCardDef Assets.backpack
  sledge <- genPlayerCardDef Assets.woodenSledge
  spare <- genPlayerCardDef Assets.backpack
  push $ CreateAssetAt aid pack (InPlayArea iid)
  push $ PlaceUnderneath (AssetTarget aid) [sledge]
  push $ PutCardOnTopOfDeck iid (Deck.InvestigatorDeck iid) spare
  runMessages "fixture" Nothing
  pure ["  the spare Backpack is the top card of your deck"]

-- | Dyer with a secret, Danforth with horror, and two horror already healed.
stageThisWasYourIdea :: GameAppT [Text]
stageThisWasYourIdea = withInvestigator \iid -> do
  setStore eoteDyerHealedDanforthKey (2 :: Int)
  void $ putIntoPlay iid Assets.professorWilliamDyerProfessorOfGeology
  danforth <- putIntoPlay iid Assets.danforthBrilliantStudent
  for_ danforth \aid -> push $ PlaceHorror CampaignSource (AssetTarget aid) 4
  runMessages "fixture" Nothing
  pure ["  use Dyer's action on Danforth to heal the last 2 horror"]

-- | Four Memorials assets in play, a fifth in hand.
stageMemorials :: GameAppT [Text]
stageMemorials = withInvestigator \iid -> do
  for_
    [Assets.claypoolsFurs, Assets.collectedWorksOfPoe, Assets.cookiesCustom32, Assets.ellsworthsBoots]
    $ putIntoPlay iid
  addToHandFromNowhere iid Assets.kenslersLog
  runMessages "fixture" Nothing
  pure ["  play Kensler's Log from your hand"]

-- | Dynamite in play and two Giant Albino Penguins sharing your location.
stageWukWukBoom :: GameAppT [Text]
stageWukWukBoom = withInvestigator \iid -> do
  void $ putIntoPlay iid Assets.dynamite
  mlid <- getLocationOf iid
  for_ mlid \lid -> replicateM_ 2 do
    card <- genPlayerCardDef Enemies.giantAlbinoPenguin
    (_, createMsg) <- createEnemyAt card lid Nothing
    push createMsg
  runMessages "fixture" Nothing
  pure ["  use the Dynamite action; 3 damage kills both penguins at once"]

-- | Dr. Mala Sinha taken into every scenario played so far.
stageFriendsForever :: GameAppT [Text]
stageFriendsForever = withInvestigator \iid -> do
  -- Put her on the table too, so it is visible who came along.
  void $ putIntoPlay iid Assets.drMalaSinhaDaringPhysician
  let sinha = toCardCode Assets.drMalaSinhaDaringPhysician
  setStore eotePartnersBroughtKey [sinha]
  played <- selectOne TheScenario
  let scenarios = maybe [] pure played
  setStore eoteScenariosPlayedKey scenarios
  setStore eoteScenariosWithPartnerKey scenarios
  runMessages "fixture" Nothing
  pure ["  Dr. Mala Sinha is in your play area and is Resolute"]

-- | Replace the campaign chaos bag's Frost tokens with exactly @n@ of them.
withFrostTokens :: Int -> CampaignAttrs -> CampaignAttrs
withFrostTokens n attrs =
  attrs {campaignChaosBag = replicate n FrostToken <> filter (/= FrostToken) (campaignChaosBag attrs)}

-- | Mark Dr. Mala Sinha as having confronted her demons.
withResolutePartner :: CampaignAttrs -> CampaignAttrs
withResolutePartner attrs =
  attrs
    & logL
    . partnersL
    . ix (toCardCode Assets.drMalaSinhaDaringPhysician)
    . statusL
    .~ Resolute

{- | Put The Eye of Ravens under the investigator's control with four keys already
tallied as shifted this turn, so shifting it is the fifth.
-}
stageFifthShift :: GameAppT [Text]
stageFifthShift = withInvestigator \iid -> do
  card <- genPlayerCardDef Keys.theEyeOfRavens
  push $ CreateScarletKeyAt card (AttachedToInvestigator iid)
  runMessages "fixture" Nothing
  -- Seeded after the key exists so its cascade cannot overwrite the tally.
  setStore shiftedKeysKey (["c09520", "c09521", "c09522", "c09523"] :: [Text])
  runMessages "fixture" Nothing
  pure ["  you control The Eye of Ravens; use its fast ability to shift it"]

{- | Four cuisine moments banked and the investigator standing one move OFF Cafe
Luna, so walking back onto it is the fifth.

Dancing Mad starts everyone ON Cafe Luna, and that starting placement already
counts (it routes through the ordinary EnterLocation path), so the store is
rewritten WITHOUT Havana and the investigator is stepped next door - otherwise the
game would arrive with the achievement already earned.
-}
stageLastMeal :: GameAppT [Text]
stageLastMeal = withInvestigator \iid -> do
  setStore cuisineKey (["Marrakesh", "BuenosAires", "Tokyo", "KualaLumpur"] :: [Text])
  runMessages "fixture" Nothing
  elsewhere <- selectOne $ LocationWithTitle "El Malec\243n"
  case elsewhere of
    Nothing -> pure ["  ! could not find El Malecon to step off Cafe Luna"]
    Just lid -> do
      push $ PlaceInvestigator iid (AtLocation lid)
      runMessages "fixture" Nothing
      pure ["  you are at El Malecon, one move from Cafe Luna"]

{- | Both copies of Desi at the investigator's location, softened so one Dynamite
Blast (3 damage to everything there) kills both at once, with the Blast in hand.

Desi's health is 2 plus 2 per player, and while act 1 or 2 is in play his forced
ability replaces any damage with an automatic evade - so the act deck is advanced
to act 3 first, otherwise the Blast would simply bounce off both of them.
-}
stageBothDesis :: GameAppT [Text]
stageBothDesis = withInvestigator \iid -> do
  -- Act 3 (False Light): Desi's damage-to-evade ability only applies on acts 1-2.
  falseLight <- genPlayerCardDef Acts.falseLight
  push $ SetCurrentActDeck 1 [falseLight]
  runMessages "fixture" Nothing

  mlid <- getLocationOf iid
  case mlid of
    Nothing -> pure ["  ! the investigator is nowhere - nothing staged"]
    Just lid -> do
      for_ [Enemies.desiderioDelgadoAlvarez106, Enemies.desiderioDelgadoAlvarez107] \def -> do
        card <- getSetAsideCardMaybe def >>= maybe (genPlayerCardDef def) pure
        (eid, msg) <- createEnemyAt card lid Nothing
        push msg
        runMessages "fixture" Nothing
        -- Health is only readable once the enemy is actually in play.
        health <- fromMaybe 4 <$> field EnemyHealth eid
        push $ PlaceDamage ScenarioSource (EnemyTarget eid) (max 0 (health - 3))
        runMessages "fixture" Nothing

      {- Dynamite Blast straight into hand. The messages that add a card from
      nowhere (AddToHand, PutCardOnTopOfDeck) do not survive into the persisted
      game here, and Roland's own copy may already have been drawn or shuffled
      out of reach, so the card is minted and placed by a direct state edit. -}
      dynamite <- genPlayerCardDef Events.dynamiteBlast
      ref <- asks appGame
      liftIO $ modifyIORef' ref $ overInvestigator iid \attrs ->
        attrs {investigatorHand = dynamite : investigatorHand attrs}
      pure
        [ "  both Desis are at your location, each 3 damage from death"
        , "  Dynamite Blast is in your hand"
        ]

{- | The Light of Pharos on a bearer with exactly 1 remaining health, with the key
two thirds of the way through the Catacombs' steal.

Dogs of War v. III puts the key on The Beast; the Catacombs of Kom El Shoqafa
hands it over once the key carries 3 resources, so two are placed in advance.
-}
stageWoundedBearer :: GameAppT [Text]
stageWoundedBearer = do
  mBearer <- selectOne $ enemyIs Enemies.theBeastInACowlOfCrimsonWolfInSheepsClothing
  mKey <- selectOne $ scarletKeyIs Keys.theLightOfPharos
  case (mBearer, mKey) of
    (Just eid, Just kid) -> do
      health <- fromMaybe 1 <$> field EnemyHealth eid
      damage <- field EnemyDamage eid
      push $ PlaceDamage ScenarioSource (EnemyTarget eid) (max 0 (health - damage - 1))
      push $ PlaceTokens ScenarioSource (ScarletKeyTarget kid) Token.Resource 2
      runMessages "fixture" Nothing
      remaining <- field EnemyRemainingHealth eid
      pure
        [ "  The Beast has " <> tshow (fromMaybe 0 remaining) <> " remaining health"
        , "  The Light of Pharos carries 2 of the 3 resources it needs"
        ]
    _ -> pure ["  ! The Beast or The Light of Pharos is not in play - nothing staged"]

-- | Edit one investigator's attrs in place.
overInvestigator :: InvestigatorId -> (InvestigatorAttrs -> InvestigatorAttrs) -> Game -> Game
overInvestigator iid f g =
  g
    { gameEntities =
        (gameEntities g)
          { entitiesInvestigators =
              Map.adjust (overAttrs f) iid (entitiesInvestigators (gameEntities g))
          }
    }

-- Small conveniences shared by the stagers above.

withInvestigator :: (InvestigatorId -> GameAppT [Text]) -> GameAppT [Text]
withInvestigator f = selectOne Anyone >>= maybe (pure ["  no investigator - nothing staged"]) f

{- | Mint a card and register it. Cards made outside the engine's CardGen are
absent from 'gameCards', and any later 'getCard' on them throws.
-}
genPlayerCardDef :: CardDef -> GameAppT Card
genPlayerCardDef def = do
  -- CardId has no Random instance; it is a UUID newtype, so make one directly.
  cardId <- unsafeMakeCardId <$> getRandom
  let card = lookupCard (toCardCode def) cardId
  ref <- asks appGame
  liftIO $ modifyIORef' ref \g -> g {gameCards = Map.insert cardId card (gameCards g)}
  pure card

putIntoPlay :: InvestigatorId -> CardDef -> GameAppT (Maybe AssetId)
putIntoPlay iid def = do
  aid <- getRandom
  card <- genPlayerCardDef def
  push $ CreateAssetAt aid card (InPlayArea iid)
  pure (Just aid)

addToHandFromNowhere :: InvestigatorId -> CardDef -> GameAppT ()
addToHandFromNowhere iid def = do
  card <- genPlayerCardDef def
  push $ AddToHand iid [card]

getSetAsideCardMaybe :: CardDef -> GameAppT (Maybe Card)
getSetAsideCardMaybe def = listToMaybe <$> select (SetAsideCardMatch $ cardIs def)

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
    when (fxCampaign fx == edgeOfTheEarthId) (reseedPartners app)
    if gameGameState game == IsOver
      || (gamePhase game == InvestigationPhase && isJust (gameTurnPlayerInvestigatorId game))
      then pure ()
      else case parkedQuestion game of
        Nothing -> warn "  ! setup parked with no question"
        -- The prompt this fixture wants to be handed over on: leave it parked.
        Just (_, q) | maybe False (`questionMentions` q) (fxStopAtLabel fx) -> pure ()
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
            {- The settings answer installs a WHOLE new campaign log, and the first
            campaign step runs in the same drain right behind it - so anything the
            log needs has to be in the log we send, not patched in afterwards.
            Edge of the Earth's partner roster is seeded by StartCampaign and wiped
            here; every partner-aware scenario setup then throws without it. -}
            PickCampaignSettings -> do
              let base = makeCampaignLog (CampaignSettings (fxRecords fx) (fxCounts fx) mempty [])
              let log' =
                    if fxCampaign fx == edgeOfTheEarthId then withExpeditionTeam base else base
              pure (Handled [SetCampaignLog log'], True)
            _ -> do
              answer <- case question of
                PickScenarioSettings -> pure $ StandaloneSettingsAnswer []
                _ -> runGameApp app (decideAi rolandAiState qpid q)
              r <- handleAnswerPure game qpid answer
              pure (r, False)
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
              -- Something in the mid-campaign start path clears the partner roster
              -- StartCampaign seeds, and every partner-aware scenario setup throws
              -- without it. Rather than guess which message drops it, put it back
              -- before each step; it is idempotent.
              when (fxCampaign fx == edgeOfTheEarthId) (reseedPartners app)
              loop (stepsLeft - 1)

{- | Whether a parked question mentions a label. Choice labels are i18n keys
(@labeled' "overthrow"@ becomes @$<scope>.overthrow@), so the encoded question is
searched for the bare label rather than reconstructing the scope here.
-}

-- | Encoded form of a parked question, for substring checks.
tshowQuestion :: Question Message -> Text
tshowQuestion = decodeUtf8 . BSL.toStrict . encode

questionMentions :: Text -> Question Message -> Bool
questionMentions label q = label `T.isInfixOf` decodeUtf8 (BSL.toStrict (encode q))

{- | Put the expedition team back into the campaign log, exactly as
Edge of the Earth's StartCampaign does. Every partner-aware scenario setup reads
this roster, and throws outright when a partner is missing from it.
-}
reseedPartners :: GameApp -> IO ()
reseedPartners app =
  modifyIORef' (appGame app) $ modeL %~ first (overAttrs (logL %~ withExpeditionTeam))

-- | Add the whole expedition team to a campaign log, as StartCampaign does.
withExpeditionTeam :: CampaignLog -> CampaignLog
withExpeditionTeam = \l -> foldl' (flip addPartner) l expeditionTeam
 where
  addPartner def = partnersL . at (toCardCode def) ?~ CampaignLogPartner 0 0 Safe

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
  let edited = game {gameSettings = settings} & modeL %~ first (overAttrs (fxCampaignEdit fx))
  pure (edited, queue, extra)

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
      base = newCampaign (fxCampaign fx) (fxScenario fx) seed 1 (fxDifficulty fx) False
      settings =
        (gameSettings base) {settingsUltimatumsAndBoons = Set.fromList (fxUltimatums fx)}
      game0 = base {gameSettings = settings} & modeL %~ first (overAttrs (fxCampaignSeed fx))
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
      , fxLabel fx <> "  (" <> achievementName (fxAchievement fx) <> ")"
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
    , "  Inserts one solo game per The Scarlet Keys achievement into the database"
    , "  named by DATABASE_URL, owned by the given user, and prints how to finish each."
    , "  The earlier campaigns' fixtures are only built when named with --only."
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
          then defaultFixtures
          else filter ((`elem` optOnly opts) . achievementName . fxAchievement) allFixtures

  runNoLoggingT $ withPostgresqlPool connStr 1 \pool -> liftIO do
    -- One fixture that cannot be staged must not take the rest of the run with it.
    reports <- traverse (buildOne pool tracer (optEmail opts)) selected
    putStrLn "==== achievement fixtures ===="
    traverse_ (putStrLn . T.unpack) (concat reports)

buildOne :: ConnectionPool -> Tracer -> Text -> Fixture -> IO [Text]
buildOne pool tracer email fx =
  try @SomeException (forceReport =<< runSqlPool (insertFixture tracer email fx) pool) >>= \case
    Right report -> pure report
    Left err ->
      pure
        [ ""
        , fxLabel fx <> "  (" <> achievementName (fxAchievement fx) <> ")"
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
