module Arkham.Game.Base where

import Arkham.Prelude

import Arkham.Entities
import Arkham.Id
import Arkham.History
import Arkham.Enemy
import Arkham.Investigator

data Game = Game
  { gamePhaseHistory :: HashMap InvestigatorId History
  , gameTurnHistory :: HashMap InvestigatorId History
  , gameRoundHistory :: HashMap InvestigatorId History
  , gameInitialSeed :: Int
  , gameSeed :: Int
  , gameParams :: GameParams
  , gameWindowDepth :: Int
  , -- Active Scenario/Campaign
    gameMode :: GameMode
  , -- Entities
    gameEntities :: Entities
  , gameEncounterDiscardEntities :: Entities
  , gameInHandEntities :: HashMap InvestigatorId Entities
  , gameInDiscardEntities :: HashMap InvestigatorId Entities
  , gameInSearchEntities :: Entities
  , gameEnemiesInVoid :: EntityMap Enemy
  , -- Player Details
    gamePlayerCount :: Int -- used for determining if game should start
  , gameActiveInvestigatorId :: InvestigatorId
  , gameTurnPlayerInvestigatorId :: Maybe InvestigatorId
  , gameLeadInvestigatorId :: InvestigatorId
  , gamePlayerOrder :: [InvestigatorId] -- For "in player order"
  , -- Game Details
    gamePhase :: Phase
  , gameSkillTest :: Maybe SkillTest
  , gameFocusedCards :: [Card]
  , gameFoundCards :: HashMap Zone [Card]
  , gameFocusedTargets :: [Target]
  , gameFocusedTokens :: [Token]
  , gameActiveCard :: Maybe Card
  , gameRemovedFromPlay :: [Card]
  , gameGameState :: GameState
  , gameSkillTestResults :: Maybe SkillTestResultsData
  , gameEnemyMoving :: Maybe EnemyId
  , -- Active questions
    gameQuestion :: HashMap InvestigatorId (Question Message)
  }
  deriving stock (Eq, Show)

$(deriveJSON defaultOptions ''Game)

makeLensesWith suffixedFields ''Game

