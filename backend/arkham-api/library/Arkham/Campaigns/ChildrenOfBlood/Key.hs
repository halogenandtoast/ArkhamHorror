module Arkham.Campaigns.ChildrenOfBlood.Key where

import Arkham.Prelude

data ChildrenOfBloodKey
  = InvestigatorsFailedToStopJuliaStern
  | InvestigatorsSparedJuliaStern
  | InvestigatorsKilledJuliaStern
  | InvestigatorsDidNotCompleteTheirSearch
  | InvestigatorsCompletedTheirSearch
  | InvestigatorsLeftZburamoarteAlive
  | InvestigatorsDefeatedZburamoarte
  | InvestigatorsWereLeftToTheCultsMercy
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
