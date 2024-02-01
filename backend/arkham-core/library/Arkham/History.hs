module Arkham.History (module Arkham.History, module Arkham.History.Types) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Enemy.Types.Attrs
import Arkham.History.Types
import Arkham.Id
import Arkham.SkillType
import Arkham.Target
import Data.Map.Strict qualified as Map

data DefeatedEnemyAttrs = DefeatedEnemyAttrs
  { defeatedEnemyAttrs :: EnemyAttrs
  , defeatedEnemyHealth :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, NoThunks)

data History = History
  { historyTreacheriesDrawn :: [CardCode]
  , historyDealtDamageTo :: [Target]
  , historyEnemiesDefeated :: [DefeatedEnemyAttrs]
  , historyMoved :: Bool
  , historyLocationsSuccessfullyInvestigated :: Set LocationId
  , historySuccessfulExplore :: Bool
  , historyActionsCompleted :: Int
  , historySkillTestsPerformed :: [[SkillType]]
  , historyPlayedCards :: [Card]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, NoThunks)

instance Semigroup History where
  h <> g =
    History
      { historyTreacheriesDrawn =
          historyTreacheriesDrawn h
            <> historyTreacheriesDrawn g
      , historyDealtDamageTo = historyDealtDamageTo h <> historyDealtDamageTo g
      , historyEnemiesDefeated =
          historyEnemiesDefeated h
            <> historyEnemiesDefeated g
      , historyMoved = historyMoved h || historyMoved g
      , historyLocationsSuccessfullyInvestigated =
          historyLocationsSuccessfullyInvestigated h
            <> historyLocationsSuccessfullyInvestigated g
      , historySuccessfulExplore =
          historySuccessfulExplore h
            || historySuccessfulExplore g
      , historyActionsCompleted =
          historyActionsCompleted h
            + historyActionsCompleted g
      , historySkillTestsPerformed =
          historySkillTestsPerformed h
            <> historySkillTestsPerformed g
      , historyPlayedCards = historyPlayedCards h <> historyPlayedCards g
      }

instance Monoid History where
  mempty = History [] [] [] False mempty False 0 [] []

insertHistory
  :: InvestigatorId
  -> History
  -> Map InvestigatorId History
  -> Map InvestigatorId History
insertHistory = Map.insertWith (<>)
