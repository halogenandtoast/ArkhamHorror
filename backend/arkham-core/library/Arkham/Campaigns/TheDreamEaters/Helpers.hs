module Arkham.Campaigns.TheDreamEaters.Helpers where

import Arkham.Campaigns.TheDreamEaters.Meta
import Arkham.Classes.HasGame
import Arkham.Helpers.Campaign
import Arkham.Prelude

getIsFullCampaign :: HasGame m => m Bool
getIsFullCampaign = do
  meta <- getCampaignMeta
  pure $ campaignMode meta == FullMode

getIsPartialCampaign :: HasGame m => CampaignPart -> m Bool
getIsPartialCampaign campaignPart = do
  meta <- getCampaignMeta
  pure $ campaignMode meta == PartialMode campaignPart

getIsTheWebOfDreams :: HasGame m => m Bool
getIsTheWebOfDreams = getIsPartialCampaign TheWebOfDreams

getIsTheDreamQuest :: HasGame m => m Bool
getIsTheDreamQuest = getIsPartialCampaign TheDreamQuest
