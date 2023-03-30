module Arkham.ActiveCost.Base where

import Arkham.Prelude

import Arkham.Card
import Arkham.Cost
import Arkham.Id
import Arkham.Token
import Arkham.Ability.Types
import Arkham.Window ( Window )

data ActiveCost = ActiveCost
  { activeCostId :: ActiveCostId
  , activeCostCosts :: Cost
  , activeCostPayments :: Payment
  , activeCostTarget :: ActiveCostTarget
  , activeCostWindows :: [Window]
  , activeCostInvestigator :: InvestigatorId
  , activeCostSealedTokens :: [Token]
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
