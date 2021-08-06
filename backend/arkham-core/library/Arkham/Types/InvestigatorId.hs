module Arkham.Types.InvestigatorId where

import Arkham.Prelude

import Arkham.Types.Card.CardCode

newtype InvestigatorId = InvestigatorId { unInvestigatorId :: CardCode }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype InScenarioInvestigatorId = InScenarioInvestigatorId { unInScenarioInvestigatorId :: InvestigatorId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype DefeatedInvestigatorId = DefeatedInvestigatorId { unDefeatedInvestigatorId :: InvestigatorId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype PreyId = PreyId { unPreyId :: InvestigatorId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype OwnerId = OwnerId { unOwnerId :: InvestigatorId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)
