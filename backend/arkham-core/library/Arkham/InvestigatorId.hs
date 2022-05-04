module Arkham.InvestigatorId where

import Arkham.Prelude

import Arkham.Card.CardCode

newtype InvestigatorId = InvestigatorId { unInvestigatorId :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype InScenarioInvestigatorId = InScenarioInvestigatorId { unInScenarioInvestigatorId :: InvestigatorId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype DefeatedInvestigatorId = DefeatedInvestigatorId { unDefeatedInvestigatorId :: InvestigatorId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)
