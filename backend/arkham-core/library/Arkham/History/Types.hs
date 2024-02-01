module Arkham.History.Types where

import Arkham.Prelude

data HistoryType
  = PhaseHistory
  | RoundHistory
  | TurnHistory
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks)
