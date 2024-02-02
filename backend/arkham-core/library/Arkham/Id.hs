{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Id where

import Arkham.Prelude

import Arkham.Card.CardCode

deriving via AllowThunk UUID instance NoThunks UUID

newtype MovementId = MovementId UUID
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random, NoThunks, NFData)

newtype PlayerId = PlayerId {unPlayerId :: UUID}
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random, NoThunks, NFData)

newtype ActId = ActId {unActId :: CardCode}
  deriving stock (Data)
  deriving newtype
    (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey, NoThunks, NFData)

newtype AgendaId = AgendaId {unAgendaId :: CardCode}
  deriving stock (Data)
  deriving newtype
    (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey, NoThunks, NFData)

newtype AssetId = AssetId {unAssetId :: UUID}
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random, NoThunks, NFData)

newtype CampaignId = CampaignId {unCampaignId :: Text}
  deriving stock (Data)
  deriving newtype
    (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, IsString, NoThunks, NFData)

newtype EffectId = EffectId {unEffectId :: UUID}
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random, NoThunks, NFData)

newtype EnemyId = EnemyId {unEnemyId :: UUID}
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random, NoThunks, NFData)

newtype EventId = EventId {unEventId :: UUID}
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random, NoThunks, NFData)

newtype InvestigatorId = InvestigatorId {unInvestigatorId :: CardCode}
  deriving stock (Data)
  deriving newtype
    (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, IsString, HasCardCode, NoThunks, NFData)

newtype LocationId = LocationId {unLocationId :: UUID}
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random, NoThunks, NFData)

newtype ScenarioId = ScenarioId {unScenarioId :: CardCode}
  deriving stock (Data)
  deriving newtype
    (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey, NoThunks, NFData)

newtype SkillId = SkillId {unSkillId :: UUID}
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random, NoThunks, NFData)

newtype StoryId = StoryId {unStoryId :: CardCode}
  deriving stock (Data)
  deriving newtype
    (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey, NoThunks, NFData)

newtype TreacheryId = TreacheryId {unTreacheryId :: UUID}
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random, NoThunks, NFData)

-- non entity-ids

newtype ActiveCostId = ActiveCostId {unActiveCostId :: UUID}
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random, NoThunks, NFData)

newtype TokenId = TokenId {unTokenId :: UUID}
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, NoThunks, NFData)

newtype CardDrawId = CardDrawId UUID
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random, NoThunks, NFData)

newtype BatchId = BatchId {unBatchId :: UUID}
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random, NoThunks, NFData)

class AsId a where
  type IdOf a
  asId :: a -> IdOf a

instance AsId InvestigatorId where
  type IdOf InvestigatorId = InvestigatorId
  asId = id

instance AsId AssetId where
  type IdOf AssetId = AssetId
  asId = id

instance AsId EventId where
  type IdOf EventId = EventId
  asId = id

instance AsId SkillId where
  type IdOf SkillId = SkillId
  asId = id

instance AsId EnemyId where
  type IdOf EnemyId = EnemyId
  asId = id

instance AsId LocationId where
  type IdOf LocationId = LocationId
  asId = id

instance AsId TreacheryId where
  type IdOf TreacheryId = TreacheryId
  asId = id

instance AsId EffectId where
  type IdOf EffectId = EffectId
  asId = id
