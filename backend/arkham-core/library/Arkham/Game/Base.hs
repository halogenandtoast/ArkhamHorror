{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Arkham.Game.Base where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Ability.Types
import Arkham.ActiveCost.Base
import Arkham.Campaign.Types (Campaign)
import {-# SOURCE #-} Arkham.Card
import Arkham.ChaosToken
import {-# SOURCE #-} Arkham.Entities
import Arkham.Game.Settings
import Arkham.Git (GitSha)
import Arkham.History
import Arkham.Id
import Arkham.Investigator.Types (Investigator)
import Arkham.Message
import Arkham.Modifier
import Arkham.Phase
import Arkham.Scenario.Types (Scenario)
import Arkham.SkillTest.Base
import Arkham.Target
import Arkham.Tarot
import Arkham.Zone
import Data.Aeson.Diff qualified as Diff
import Data.These

type GameMode = These Campaign Scenario
data GameState = IsPending [(Investigator, [PlayerCard])] | IsActive | IsOver
  deriving stock (Eq, Show)

data Game = Game
  { gamePhaseHistory :: Map InvestigatorId History
  , gameTurnHistory :: Map InvestigatorId History
  , gameRoundHistory :: Map InvestigatorId History
  , gameInitialSeed :: Int
  , gameSettings :: Settings
  , gameSeed :: Int
  , gameWindowDepth :: Int
  , gameRunWindows :: Bool
  , gameDepthLock :: Int
  , gameIgnoreCanModifiers :: Bool
  , -- Active Scenario/Campaign
    gameMode :: GameMode
  , -- Entities
    gameEntities :: Entities
  , gameOutOfPlayEntities :: Map OutOfPlayZone Entities
  , gameModifiers :: Map Target [Modifier]
  , gameEncounterDiscardEntities :: Entities
  , gameInHandEntities :: Map InvestigatorId Entities
  , gameInDiscardEntities :: Map InvestigatorId Entities
  , gameInSearchEntities :: Entities
  , -- Player Details
    -- used for determining if game should start
    gamePlayerCount :: Int
  , gameActiveInvestigatorId :: InvestigatorId
  , gameTurnPlayerInvestigatorId :: Maybe InvestigatorId
  , gameLeadInvestigatorId :: InvestigatorId
  , -- For "in player order"
    gamePlayerOrder :: [InvestigatorId]
  , -- Game Details
    gamePhase :: Phase
  , gamePhaseStep :: Maybe PhaseStep
  , gameSkillTest :: Maybe SkillTest
  , gameFocusedCards :: [Card]
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
    gameQuestion :: Map InvestigatorId (Question Message)
  , -- handling time warp
    gameActionCanBeUndone :: Bool
  , gameActionDiff :: [Diff.Patch]
  , gameInAction :: Bool
  , gameCards :: Map CardId Card
  , -- handling costs
    gameActiveCost :: Map ActiveCostId ActiveCost
  , -- track GitSha for debugging
    gameGitRevision :: GitSha
  , -- allow empty spaces
    gameAllowEmptySpaces :: Bool
  , -- perform tarot readings
    gamePerformTarotReadings :: Bool
  }
  deriving stock (Eq, Show)

makeLensesWith suffixedFields ''Game
