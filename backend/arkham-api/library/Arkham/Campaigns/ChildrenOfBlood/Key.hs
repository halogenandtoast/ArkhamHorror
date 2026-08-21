module Arkham.Campaigns.ChildrenOfBlood.Key where

import Arkham.Prelude

data ChildrenOfBloodKey
  = InvestigatorsFailedToStopJuliaStern
  | InvestigatorsSparedJuliaStern
  | InvestigatorsKilledJuliaStern
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
