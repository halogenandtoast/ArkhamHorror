module Arkham.History where

import Arkham.Prelude

import Arkham.Id
import Arkham.Card.CardCode
import Arkham.Target
import Data.HashMap.Strict qualified as HashMap

data HistoryType = PhaseHistory | RoundHistory | TurnHistory

data History = History
  { historyTreacheriesDrawn :: [CardCode]
  , historyDealtDamageTo :: [Target]
  , historyMoved :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Semigroup History where
  h <> g = History
    { historyTreacheriesDrawn = historyTreacheriesDrawn h
      <> historyTreacheriesDrawn g
    , historyDealtDamageTo = historyDealtDamageTo h <> historyDealtDamageTo g
    , historyMoved = historyMoved h || historyMoved g
    }

instance Monoid History where
  mempty = History [] [] False

insertHistory
  :: InvestigatorId
  -> History
  -> HashMap InvestigatorId History
  -> HashMap InvestigatorId History
insertHistory = HashMap.insertWith (<>)

