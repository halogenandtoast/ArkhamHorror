{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Campaign.Attrs where

import Arkham.Json
import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignId
import Arkham.Types.CampaignLog
import Arkham.Types.CampaignStep
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Investigator
import Arkham.Types.TokenPool
import ClassyPrelude
import Lens.Micro

data Attrs = Attrs
  { campaignId :: CampaignId
  , campaignName :: Text
  , campaignInvestigators :: HashMap Int Investigator
  , campaignDifficulty :: Difficulty
  , campaignTokenPool :: TokenPool
  , campaignLog :: CampaignLog
  , campaignSteps :: Vector CampaignStep
  , campaignStep :: Int
  }
  deriving stock (Show, Generic)

step :: Lens' Attrs Int
step = lens campaignStep $ \m x -> m { campaignStep = x }

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "campaign"
  toEncoding = genericToEncoding $ aesonOptions $ Just "campaign"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "campaign"

instance (CampaignRunner env) => RunMessage env Attrs where
  runMessage _ = pure

baseAttrs :: CampaignId -> Text -> Difficulty -> TokenPool -> Attrs
baseAttrs campaignId' name difficulty tokenPool = Attrs
  { campaignId = campaignId'
  , campaignName = name
  , campaignInvestigators = mempty
  , campaignDifficulty = difficulty
  , campaignTokenPool = tokenPool
  , campaignLog = mkCampaignLog
  , campaignSteps = mempty
  , campaignStep = 0
  }
