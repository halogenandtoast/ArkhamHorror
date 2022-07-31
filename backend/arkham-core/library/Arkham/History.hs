module Arkham.History where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Enemy.Types.Attrs
import Arkham.Id
import Arkham.Target
import Data.HashMap.Strict qualified as HashMap

data HistoryType = PhaseHistory | RoundHistory | TurnHistory

data History = History
  { historyTreacheriesDrawn :: [CardCode]
  , historyDealtDamageTo :: [Target]
  , historyEnemiesDefeated :: [EnemyAttrs]
  , historyMoved :: Bool
  , historyLocationsSuccessfullyInvestigated :: HashSet LocationId
  , historySuccessfulExplore :: Bool
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
    }

instance Monoid History where
  mempty = History [] [] [] False mempty False

insertHistory
  :: InvestigatorId
  -> History
  -> HashMap InvestigatorId History
  -> HashMap InvestigatorId History
insertHistory = HashMap.insertWith (<>)
