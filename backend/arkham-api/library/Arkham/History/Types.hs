module Arkham.History.Types where

import Arkham.Prelude
import GHC.OverloadedLabels

data HistoryType
  = PhaseHistory
  | RoundHistory
  | TurnHistory
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsLabel "phase" HistoryType where
  fromLabel = PhaseHistory

instance IsLabel "round" HistoryType where
  fromLabel = RoundHistory

instance IsLabel "turn" HistoryType where
  fromLabel = TurnHistory
