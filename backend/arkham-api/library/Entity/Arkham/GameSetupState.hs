module Entity.Arkham.GameSetupState where

import Arkham.Types.CampaignId
import Arkham.Types.Difficulty
import ClassyPrelude
import Json

data GameSetupState = GameSetupState
  { gssPlayerCount :: Int
  , gssPlayers :: HashMap Int String
  , gssCampaignId :: CampaignId
  , gssDifficulty :: Difficulty
  }
  deriving stock (Generic, Show)

instance ToJSON GameSetupState where
  toJSON = genericToJSON $ aesonOptions $ Just "gss"
  toEncoding = genericToEncoding $ aesonOptions $ Just "gss"

instance FromJSON GameSetupState where
  parseJSON = genericParseJSON $ aesonOptions $ Just "gss"

