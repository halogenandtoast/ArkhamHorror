module AH3e.Types.Ids where

import AH3e.Prelude

newtype PlayerId = PlayerId Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype InvestigatorId = InvestigatorId Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype SpaceId = SpaceId Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype NeighborhoodId = NeighborhoodId Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype CardCode = CardCode Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype CardId = CardId Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Enum, Num)

newtype ArchiveNumber = ArchiveNumber Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Num)

newtype ScenarioCode = ScenarioCode Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype Trait = Trait Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)
