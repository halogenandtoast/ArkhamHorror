module Arkham.ModifierData (
  module Arkham.ModifierData,
) where

import Arkham.Prelude

import Arkham.Id
import Arkham.Json
import Arkham.Modifier
import Arkham.SkillType

newtype ModifierData = ModifierData {mdModifiers :: [Modifier]}
  deriving stock (Show, Eq, Generic)

instance ToJSON ModifierData where
  toJSON = genericToJSON $ aesonOptions $ Just "md"
  toEncoding = genericToEncoding $ aesonOptions $ Just "md"

data LocationMetadata = LocationMetadata
  { lmConnectedLocations :: [LocationId]
  , lmInvestigators :: [InvestigatorId]
  , lmEnemies :: [EnemyId]
  , lmTreacheries :: [TreacheryId]
  , lmAssets :: [AssetId]
  , lmEvents :: [EventId]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON LocationMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "lm"
  toEncoding = genericToEncoding $ aesonOptions $ Just "lm"

newtype ConnectionData = ConnectionData {cdConnectedLocations :: [LocationId]}
  deriving stock (Show, Eq, Generic)

instance ToJSON ConnectionData where
  toJSON = genericToJSON $ aesonOptions $ Just "cd"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cd"

data EnemyMetadata = EnemyMetadata
  { emEngagedInvestigators :: [InvestigatorId]
  , emTreacheries :: [TreacheryId]
  , emAssets :: [AssetId]
  , emEvents :: [EventId]
  , emSkills :: [SkillId]
  , emModifiers :: [Modifier]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON EnemyMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "em"
  toEncoding = genericToEncoding $ aesonOptions $ Just "em"

data AssetMetadata = AssetMetadata
  { amEvents :: [EventId]
  , amAssets :: [AssetId]
  , amTreacheries :: [TreacheryId]
  , amModifiers :: [Modifier]
  , amPermanent :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AssetMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "am"
  toEncoding = genericToEncoding $ aesonOptions $ Just "am"

data SkillTestMetadata = SkillTestMetadata
  { stmModifiedSkillValue :: Int
  , stmModifiedDifficulty :: Int
  , stmSkills :: [SkillType]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SkillTestMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "stm"
  toEncoding = genericToEncoding $ aesonOptions $ Just "stm"

data ActMetadata = ActMetadata
  { actmTreacheries :: [TreacheryId]
  , actmModifiers :: [Modifier]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ActMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "actm"
  toEncoding = genericToEncoding $ aesonOptions $ Just "actm"

data AgendaMetadata = AgendaMetadata
  { agendamTreacheries :: [TreacheryId]
  , agendamModifiers :: [Modifier]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AgendaMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "agendam"
  toEncoding = genericToEncoding $ aesonOptions $ Just "agendam"
