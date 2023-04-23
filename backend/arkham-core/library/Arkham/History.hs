module Arkham.History where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Enemy.Types.Attrs
import Arkham.Id
import Arkham.Target
import Data.Map.Strict qualified as Map

data HistoryType = PhaseHistory | RoundHistory | TurnHistory

data History = History
  { historyTreacheriesDrawn :: [CardCode]
  , historyDealtDamageTo :: [Target]
  , historyEnemiesDefeated :: [EnemyAttrs]
  , historyMoved :: Bool
  , historyLocationsSuccessfullyInvestigated :: Set LocationId
  , historySuccessfulExplore :: Bool
  , historyActionsCompleted :: Int
  , historySkillTestsPerformed :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Semigroup History where
  h <> g = History
    { historyTreacheriesDrawn = historyTreacheriesDrawn h
      <> historyTreacheriesDrawn g
    , historyDealtDamageTo = historyDealtDamageTo h <> historyDealtDamageTo g
    , historyEnemiesDefeated = historyEnemiesDefeated h
      <> historyEnemiesDefeated g
    , historyMoved = historyMoved h || historyMoved g
    , historyLocationsSuccessfullyInvestigated =
      historyLocationsSuccessfullyInvestigated h
        <> historyLocationsSuccessfullyInvestigated g
    , historySuccessfulExplore = historySuccessfulExplore h
      || historySuccessfulExplore g
    , historyActionsCompleted = historyActionsCompleted h
      + historyActionsCompleted g
    , historySkillTestsPerformed = historySkillTestsPerformed h
      + historySkillTestsPerformed g
    }

instance Monoid History where
  mempty = History [] [] [] False mempty False 0 0

insertHistory
  :: InvestigatorId
  -> History
  -> Map InvestigatorId History
  -> Map InvestigatorId History
insertHistory = Map.insertWith (<>)
