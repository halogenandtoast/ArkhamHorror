module Arkham.Types.Campaign where

import Arkham.Prelude

import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.ScenarioId
import Arkham.Types.Token
import Arkham.Types.Campaign.Attrs
import Arkham.Types.Campaign.Campaigns
import Arkham.Types.Campaign.Runner
import Arkham.Types.Difficulty

data Campaign
  = NightOfTheZealot' NightOfTheZealot
  | ReturnToNightOfTheZealot' ReturnToNightOfTheZealot
  | TheDunwichLegacy' TheDunwichLegacy
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance CampaignRunner env => RunMessage env Campaign

instance HasRecord Campaign where
  hasRecord key = hasRecord key . campaignLog . toAttrs
  hasRecordSet key = hasRecordSet key . campaignLog . toAttrs

instance HasSet CompletedScenarioId env Campaign where
  getSet = getSet . toAttrs

instance HasList CampaignStoryCard env Campaign where
  getList = getList . toAttrs

instance Entity Campaign where
  type EntityId Campaign = CampaignId
  type EntityAttrs Campaign = CampaignAttrs

instance NamedEntity Campaign where
  toName = toName . toAttrs

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
difficultyOf = campaignDifficulty . toAttrs

chaosBagOf :: Campaign -> [Token]
chaosBagOf = campaignChaosBag . toAttrs
