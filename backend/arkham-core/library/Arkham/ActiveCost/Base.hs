module Arkham.ActiveCost.Base where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Cost
import Arkham.Id
import Arkham.Window (Window)

data ActiveCost = ActiveCost
  { activeCostId :: ActiveCostId
  , activeCostCosts :: Cost
  , activeCostPayments :: Payment
  , activeCostTarget :: ActiveCostTarget
  , activeCostWindows :: [Window]
  , activeCostInvestigator :: InvestigatorId
  , activeCostSealedChaosTokens :: [ChaosToken]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data IsPlayAction = IsPlayAction | NotPlayAction
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActiveCostTarget
  = ForCard IsPlayAction Card
  | ForAbility Ability
  | ForCost Card -- used when the active cost will not determine an effect
  | ForAdditionalCost BatchId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
