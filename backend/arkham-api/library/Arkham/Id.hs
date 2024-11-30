module Arkham.Id where

import Arkham.Prelude

import Arkham.Card.CardCode

data NamedId = NamedId
  { nuName :: Text
  , nuId :: Int
  }
  deriving stock (Show, Generic, Ord, Eq, Data)
  deriving anyclass (ToJSON, FromJSON, FromJSONKey, ToJSONKey)

namedId :: IdGen m => Text -> m NamedId
namedId name = NamedId name <$> genId

class Monad m => IdGen m where
  genId :: Coercible Int a => m a

genIds :: (IdGen m, Coercible Int a) => m [a]
genIds = (:) <$> genId <*> genIds

class Monad m => HasIdGen m where
  idGenerator :: m (IORef Int)

newtype MovementId = MovementId Int
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype SkillTestId = SkillTestId Int
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype PlayerId = PlayerId {unPlayerId :: UUID}
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype ActId = ActId {unActId :: CardCode}
  deriving stock Data
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)

newtype AgendaId = AgendaId {unAgendaId :: CardCode}
  deriving stock Data
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)

newtype AssetId = AssetId {unAssetId :: Int}
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype CampaignId = CampaignId {unCampaignId :: Text}
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, IsString)

newtype EffectId = EffectId {unEffectId :: Int}
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype EnemyId = EnemyId {unEnemyId :: Int}
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype EventId = EventId {unEventId :: Int}
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype InvestigatorId = InvestigatorId {unInvestigatorId :: CardCode}
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, IsString, HasCardCode)

newtype LocationId = LocationId {unLocationId :: Int}
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype ScenarioId = ScenarioId {unScenarioId :: CardCode}
  deriving stock Data
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)

newtype SkillId = SkillId {unSkillId :: Int}
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype StoryId = StoryId {unStoryId :: CardCode}
  deriving stock Data
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)

newtype TreacheryId = TreacheryId {unTreacheryId :: Int}
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

-- non entity-ids

newtype ActiveCostId = ActiveCostId {unActiveCostId :: Int}
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype TokenId = TokenId {unTokenId :: Int}
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord)

newtype CardDrawId = CardDrawId Int
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

newtype BatchId = BatchId {unBatchId :: Int}
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, Random)

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
