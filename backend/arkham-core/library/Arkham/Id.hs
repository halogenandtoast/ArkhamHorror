module Arkham.Id where

import Arkham.Prelude

import Arkham.Card.CardCode

newtype ActId = ActId { unActId :: CardCode }
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)

newtype AgendaId = AgendaId { unAgendaId :: CardCode }
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)

newtype AssetId = AssetId { unAssetId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype CampaignId = CampaignId { unCampaignId :: Text }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, IsString)

newtype EffectId = EffectId { unEffectId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype EnemyId = EnemyId { unEnemyId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype EventId = EventId { unEventId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype InvestigatorId = InvestigatorId { unInvestigatorId :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, IsString)

newtype LocationId = LocationId { unLocationId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype ScenarioId = ScenarioId { unScenarioId :: CardCode }
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)

newtype SkillId = SkillId { unSkillId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype TreacheryId = TreacheryId { unTreacheryId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

-- non entity-ids

newtype ActiveCostId = ActiveCostId { unActiveCostId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype TokenId = TokenId { unTokenId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord)

newtype CardDrawId = CardDrawId UUID
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)
