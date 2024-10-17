module Arkham.Campaigns.TheDreamEaters.Helpers where

import Arkham.Campaigns.TheDreamEaters.Meta
import Arkham.Classes.HasGame
import Arkham.Helpers.Campaign
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Prelude

getIsFullCampaign :: HasGame m => m Bool
getIsFullCampaign = do
  standalone <- getIsStandalone
  if standalone
    then pure False
    else do
      meta <- getCampaignMeta
      pure $ campaignMode meta == FullMode

getIsPartialCampaign :: HasGame m => CampaignPart -> m Bool
getIsPartialCampaign campaignPart = do
  standalone <- getIsStandalone
  if standalone
    then pure True
    else do
      meta <- getCampaignMeta
      pure $ campaignMode meta == PartialMode campaignPart

getIsTheWebOfDreams :: HasGame m => m Bool
getIsTheWebOfDreams = getIsPartialCampaign TheWebOfDreams

getIsTheDreamQuest :: HasGame m => m Bool
getIsTheDreamQuest = getIsPartialCampaign TheDreamQuest

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theDreamEaters" a
