module Arkham.Types.Card
  ( ArkhamCardCode(..)
  , ArkhamCard(..)
  , ArkhamPlayerCard(..)
  , ArkhamEncounterCard(..)
  , _PlayerCard
  )
where

import Arkham.Types.Skill
import ClassyPrelude
import Json
import Lens.Micro

newtype ArkhamCardCode = ArkhamCardCode { unArkhamCardCode :: Text }
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

data ArkhamPlayerCard = ArkhamPlayerCard
  { apcName :: Text
  , apcCost :: Maybe Int
  , apcCode :: ArkhamCardCode
  , apcImage :: Text
  , apcIsFast :: Bool
  , apcTestIcons :: [ArkhamSkillType]
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamPlayerCard where
  toJSON = genericToJSON . aesonOptions $ Just "apc"
  toEncoding = genericToEncoding . aesonOptions $ Just "apc"

instance FromJSON ArkhamPlayerCard where
  parseJSON = genericParseJSON . aesonOptions $ Just "apc"

data ArkhamEncounterCard = ArkhamEncounterCard
  { aecName :: Text
  , aecCode :: ArkhamCardCode
  , aecImage :: Text
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamEncounterCard where
  toJSON = genericToJSON . aesonOptions $ Just "aec"
  toEncoding = genericToEncoding . aesonOptions $ Just "aec"

instance FromJSON ArkhamEncounterCard where
  parseJSON = genericParseJSON . aesonOptions $ Just "aec"

data ArkhamCard = PlayerCard ArkhamPlayerCard | EncounterCard ArkhamEncounterCard
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

_PlayerCard :: Traversal' ArkhamCard ArkhamPlayerCard
_PlayerCard f (PlayerCard c) = PlayerCard <$> f c
_PlayerCard f (EncounterCard c) = pure $ EncounterCard c
