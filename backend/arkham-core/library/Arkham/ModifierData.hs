module Arkham.ModifierData
  ( module Arkham.ModifierData
  ) where

import Arkham.Prelude

import Arkham.Id
import Arkham.Json
import Arkham.Modifier

newtype ModifierData = ModifierData { mdModifiers :: [Modifier] }
  deriving stock (Show, Eq, Generic)

instance ToJSON ModifierData where
  toJSON = genericToJSON $ aesonOptions $ Just "md"
  toEncoding = genericToEncoding $ aesonOptions $ Just "md"

data LocationMetadata = LocationMetadata
  { lmConnectedLocations :: [LocationId]
  , lmInvestigators :: Set InvestigatorId
  , lmEnemies :: Set EnemyId
  , lmTreacheries :: Set TreacheryId
  , lmAssets :: Set AssetId
  , lmEvents :: Set EventId
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON LocationMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "lm"
  toEncoding = genericToEncoding $ aesonOptions $ Just "lm"

newtype ConnectionData = ConnectionData { cdConnectedLocations :: [LocationId] }
  deriving stock (Show, Eq, Generic)

instance ToJSON ConnectionData where
  toJSON = genericToJSON $ aesonOptions $ Just "cd"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cd"

data EnemyMetadata = EnemyMetadata
  { emEngagedInvestigators :: Set InvestigatorId
  , emTreacheries :: Set TreacheryId
  , emAssets :: Set AssetId
  , emModifiers :: [Modifier]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON EnemyMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "em"
  toEncoding = genericToEncoding $ aesonOptions $ Just "em"

data AssetMetadata = AssetMetadata
  { amEvents :: Set EventId
  , amAssets :: Set AssetId
  , amModifiers :: [Modifier]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AssetMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "am"
  toEncoding = genericToEncoding $ aesonOptions $ Just "am"
