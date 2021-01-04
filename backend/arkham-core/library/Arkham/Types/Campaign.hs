module Arkham.Types.Campaign where

import Arkham.Import

import Arkham.Types.Campaign.Attrs
import Arkham.Types.Campaign.Campaigns
import Arkham.Types.Campaign.Runner
import Arkham.Types.Difficulty

data Campaign
  = NightOfTheZealot' NightOfTheZealot
  | ReturnToNightOfTheZealot' ReturnToNightOfTheZealot
  | TheDunwichLegacy' TheDunwichLegacy
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance CampaignRunner env => RunMessage env Campaign

instance HasRecord Campaign where
  hasRecord key = hasRecord key . campaignLog . campaignAttrs
  hasRecordSet key = hasRecordSet key . campaignLog . campaignAttrs

instance HasSet CompletedScenarioId env Campaign where
  getSet = getSet . campaignAttrs

allCampaigns :: HashMap CampaignId (Difficulty -> Campaign)
allCampaigns = mapFromList
  [ ("01", NightOfTheZealot' . nightOfTheZealot)
  , ("02", TheDunwichLegacy' . theDunwichLegacy)
  , ("50", ReturnToNightOfTheZealot' . returnToNightOfTheZealot)
  ]

lookupCampaign :: CampaignId -> (Difficulty -> Campaign)
lookupCampaign cid =
  fromJustNote ("Unknown campaign: " <> show cid) $ lookup cid allCampaigns

difficultyOf :: Campaign -> Difficulty
difficultyOf = campaignDifficulty . campaignAttrs

chaosBagOf :: Campaign -> [Token]
chaosBagOf = campaignChaosBag . campaignAttrs

campaignAttrs :: Campaign -> Attrs
campaignAttrs = \case
  NightOfTheZealot' attrs -> coerce attrs
  TheDunwichLegacy' attrs -> coerce attrs
  ReturnToNightOfTheZealot' attrs -> coerce attrs
