module Arkham.ActiveCost.Base where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Cost
import Arkham.Id
import Arkham.Window (Window)
import GHC.Records

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

instance HasField "id" ActiveCost ActiveCostId where
  getField = activeCostId

instance HasField "costs" ActiveCost Cost where
  getField = activeCostCosts

instance HasField "payments" ActiveCost Payment where
  getField = activeCostPayments

instance HasField "target" ActiveCost ActiveCostTarget where
  getField = activeCostTarget

instance HasField "windows" ActiveCost [Window] where
  getField = activeCostWindows

instance HasField "investigator" ActiveCost InvestigatorId where
  getField = activeCostInvestigator

instance HasField "sealedChaosTokens" ActiveCost [ChaosToken] where
  getField = activeCostSealedChaosTokens

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
