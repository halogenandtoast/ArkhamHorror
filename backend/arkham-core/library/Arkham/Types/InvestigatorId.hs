module Arkham.Types.InvestigatorId where

import Arkham.Prelude

import Arkham.Types.Card.CardCode

newtype InvestigatorId = InvestigatorId { unInvestigatorId :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype InScenarioInvestigatorId = InScenarioInvestigatorId { unInScenarioInvestigatorId :: InvestigatorId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype PreyId = PreyId { unPreyId :: InvestigatorId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype OwnerId = OwnerId { unOwnerId :: InvestigatorId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)
