{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Game.Types where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Act
import Arkham.Types.Agenda
import Arkham.Types.Asset
import Arkham.Types.Campaign
import Arkham.Types.Card
import Arkham.Types.ChaosBag
import Arkham.Types.Classes.Entity
import Arkham.Types.Difficulty
import Arkham.Types.Effect
import Arkham.Types.Enemy
import Arkham.Types.Event
import Arkham.Types.Helpers
import Arkham.Types.History
import Arkham.Types.Id
import Arkham.Types.Investigator
import Arkham.Types.Location
import Arkham.Types.Phase
import Arkham.Types.Question
import Arkham.Types.Scenario
import Arkham.Types.Skill
import Arkham.Types.SkillTest
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Treachery
import Arkham.Types.Zone
import Data.These

type GameMode = These Campaign Scenario
type EntityMap a = HashMap (EntityId a) a

data GameState = IsPending | IsActive | IsOver
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data GameParams
  = GameParams
      (Either ScenarioId CampaignId)
      Int
      [(Investigator, [PlayerCard])] -- Map for order
      Difficulty
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
    gameLocations :: EntityMap Location
  , gameInvestigators :: EntityMap Investigator
  , gameEnemies :: EntityMap Enemy
  , gameEnemiesInVoid :: EntityMap Enemy
  , gameAssets :: EntityMap Asset
  , gameActs :: EntityMap Act
  , gameAgendas :: EntityMap Agenda
  , gameTreacheries :: EntityMap Treachery
  , gameEvents :: EntityMap Event
  , gameEffects :: EntityMap Effect
  , gameSkills :: EntityMap Skill
  , -- Player Details
    gamePlayerCount :: Int -- used for determining if game should start
  , gameActiveInvestigatorId :: InvestigatorId
  , gameTurnPlayerInvestigatorId :: Maybe InvestigatorId
  , gameLeadInvestigatorId :: InvestigatorId
  , gamePlayerOrder :: [InvestigatorId] -- For "in player order"
  , -- Game Details
    gamePhase :: Phase
  , gameEncounterDeck :: Deck EncounterCard
  , gameDiscard :: [EncounterCard]
  , gameChaosBag :: ChaosBag
  , gameSkillTest :: Maybe SkillTest
  , gameUsedAbilities :: [(InvestigatorId, Ability, Int)]
  , gameResignedCardCodes :: [CardCode]
  , gameFocusedCards :: [Card]
  , gameFoundCards :: HashMap Zone [Card]
  , gameFocusedTargets :: [Target]
  , gameFocusedTokens :: [Token]
  , gameActiveCard :: Maybe Card
  , gameVictoryDisplay :: [Card]
  , gameRemovedFromPlay :: [Card]
  , gameGameState :: GameState
  , gameSkillTestResults :: Maybe SkillTestResultsData
  , gameEnemyMoving :: Maybe EnemyId
  , -- Active questions
    gameQuestion :: HashMap InvestigatorId Question
  }
  deriving stock (Eq, Show, Generic)

makeLensesWith suffixedFields ''Game
