{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
module Arkham.Game.Base where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Ability.Types
import Arkham.ActiveCost.Base
import Arkham.Campaign.Types ( Campaign )
import {-# SOURCE #-} Arkham.Card
import Arkham.Difficulty
import Arkham.Id
import Arkham.Investigator.Types ( Investigator )
import Arkham.Message
import {-# SOURCE #-} Arkham.Entities
import Arkham.Phase
import Arkham.Scenario.Types ( Scenario )
import Arkham.SkillTest.Base
import Arkham.Target
import Arkham.Modifier
import Arkham.History
import Arkham.Token
import Arkham.Zone
import Data.Aeson.Diff qualified as Diff
import Data.These

type GameMode = These Campaign Scenario
data GameState = IsPending | IsActive | IsOver
  deriving stock (Eq, Show)

data GameParams = GameParams
  { gameParamsMode :: Either ScenarioId CampaignId
  , gameParamsPlayerCount :: Int
  , gameParamsPlayersInOrder :: [(Investigator, [PlayerCard])]
  , gameParamsDifficulty :: Difficulty
  }
  deriving stock (Eq, Show)

data Game = Game
  { gamePhaseHistory :: Map InvestigatorId History
  , gameTurnHistory :: Map InvestigatorId History
  , gameRoundHistory :: Map InvestigatorId History
  , gameInitialSeed :: Int
  , gameSeed :: Int
  , gameParams :: GameParams
  , gameWindowDepth :: Int
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
  , gameSkillTest :: Maybe SkillTest
  , gameFocusedCards :: [Card]
  , gameFoundCards :: Map Zone [Card]
  , gameFocusedTokens :: [Token]
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
  }
  deriving stock (Eq, Show)

makeLensesWith suffixedFields ''Game
