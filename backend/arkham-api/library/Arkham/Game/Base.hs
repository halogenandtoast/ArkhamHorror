{-# LANGUAGE TemplateHaskell #-}

module Arkham.Game.Base where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Ability.Types
import Arkham.ActiveCost.Base
import Arkham.Campaign.Types (Campaign)
import {-# SOURCE #-} Arkham.Card (Card, CardCode, CardId)
import Arkham.ChaosToken.Types
import {-# SOURCE #-} Arkham.Entities
import Arkham.Game.Settings
import Arkham.Game.State
import Arkham.Git (GitSha)
import Arkham.History
import Arkham.Id
import Arkham.Message
import Arkham.Modifier
import Arkham.Phase
import Arkham.Scenario.Types (Scenario)
import Arkham.SkillTest.Base
import Arkham.Target
import Arkham.Tarot
import Arkham.Window
import Arkham.Zone
import Data.Aeson.Diff qualified as Diff
import GHC.Records

type GameMode = These Campaign Scenario

{- | A field that exists only at runtime: never serialized (omitField is True
so the TH-derived ToJSON drops it entirely), always 'Nothing' after parsing,
invisible to Eq/Show. JSON output is byte-identical to a record without the
field.
-}
newtype Transient a = Transient {getTransient :: Maybe a}

instance ToJSON (Transient a) where
  toJSON _ = Null
  omitField _ = True

instance FromJSON (Transient a) where
  parseJSON _ = pure (Transient Nothing)
  omittedField = Just (Transient Nothing)

instance Eq (Transient a) where
  _ == _ = True

instance Show (Transient a) where
  show _ = "Transient"

data Game = Game
  { gamePhaseHistory :: Map InvestigatorId History
  , gameTurnHistory :: Map InvestigatorId History
  , gameRoundHistory :: Map InvestigatorId History
  , gameInitialSeed :: Int
  , gameSettings :: Settings
  , gameSeed :: Int
  , gameWindowDepth :: Int
  , gameWindowStack :: Maybe [[Window]]
  , -- monotonic clock; ticks once per window-open. Parallel to gameWindowStack:
    -- gameWindowTickStack's head is the open-tick of the window currently being
    -- checked. gameEntryTicks records the tick at which each card entered play
    -- so a card cannot respond to a window that opened before it entered.
    gameWindowTick :: Int
  , gameWindowTickStack :: [Int]
  , gameEntryTicks :: Map CardId Int
  , gameRunWindows :: Bool
  , gameDepthLock :: Int
  , gameIgnoreCanModifiers :: Bool
  , -- Active Scenario/Campaign
    gameMode :: GameMode
  , -- Entities
    gameEntities :: Entities
  , gameActionRemovedEntities :: Entities -- entities removed during the current action
  , gamePlayers :: [PlayerId]
  , gameModifiers :: Map Target [Modifier]
  , gameEncounterDiscardEntities :: Entities
  , gameInHandEntities :: Map InvestigatorId Entities
  , gameInDiscardEntities :: Map InvestigatorId Entities
  , gameInSearchEntities :: Entities
  , -- Player Details
    -- used for determining if game should start
    gamePlayerCount :: Int
  , gameActiveInvestigatorId :: InvestigatorId
  , gameActivePlayerId :: PlayerId
  , gameTurnPlayerInvestigatorId :: Maybe InvestigatorId
  , gameLeadInvestigatorId :: InvestigatorId
  , -- For "in player order"
    gamePlayerOrder :: [InvestigatorId]
  , -- Game Details
    gamePhase :: Phase
  , gamePhaseStep :: Maybe PhaseStep
  , gameSkillTest :: Maybe SkillTest
  , gameFocusedCards :: [[Card]]
  , gameFocusedTarotCards :: [TarotCard]
  , gameFoundCards :: Map Zone [Card]
  , gameFocusedChaosTokens :: [ChaosToken]
  , gameActiveCard :: Maybe Card
  , gameResolvingCard :: Maybe Card
  , gameActiveAbilities :: [Ability]
  , gameRemovedFromPlay :: [Card]
  , gameInSetup :: Bool
  , gameGameState :: GameState
  , gameSkillTestResults :: Maybe SkillTestResultsData
  , gameEnemyMoving :: Maybe EnemyId
  , gameEnemyEvading :: Maybe EnemyId
  , -- Active questions
    gameQuestion :: Map PlayerId (Question Message)
  , -- handling time warp
    gameActionCanBeUndone :: Bool
  , gameActionDiff :: [Diff.Patch]
  , -- runtime-only snapshot of the state the in-flight action started from;
    -- gameActionDiff always holds a single (lazy) revert diff against it.
    -- Reconstructed from gameActionDiff on the first message after a
    -- mid-action load (see handleActionDiff).
    gameActionSnapshot :: Transient Game
  , gameInAction :: Bool
  , gameCards :: Map CardId Card
  , gameCardUses :: Map CardCode [InvestigatorId]
  , -- handling costs
    gameActiveCost :: Map ActiveCostId ActiveCost
  , -- track GitSha for debugging
    gameGitRevision :: GitSha
  , -- allow empty spaces
    gameAllowEmptySpaces :: Bool
  , -- perform tarot readings
    gamePerformTarotReadings :: Bool
  , gameCurrentBatchId :: Maybe BatchId
  , gameScenarioSteps :: Int
  , -- gameScenarioSteps value at the point each boundary message was processed
    -- (i.e. the scenario step we'd undo TO when undoing back to that boundary).
    gameUndoActionStep :: Maybe Int
  , gameUndoTurnStep :: Maybe Int
  , gameUndoPhaseStep :: Maybe Int
  , gameUndoRoundStep :: Maybe Int
  , gameAsIfAtIgnored :: Set InvestigatorId -- transient: investigators with AsIfAt suppressed during window processing
  , gameLocationOffsets :: Map LocationId (Double, Double) -- player-driven board layout overrides; shared across players
  }
  deriving stock (Eq, Show)

makeLensesWith suffixedFields ''Game

instance HasField "state" Game GameState where
  getField = gameGameState

instance HasField "players" Game [PlayerId] where
  getField = gamePlayers

instance HasField "playerCount" Game Int where
  getField = gamePlayerCount

instance HasField "seed" Game Int where
  getField = gameSeed
