{-# LANGUAGE TemplateHaskell #-}
module Arkham.Campaign where

import Arkham.Prelude

import Arkham.Campaign.Attrs
import Arkham.Campaign.Campaigns
import Arkham.Campaign.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Id
import Arkham.Name
import Arkham.Token
import Data.Aeson.TH

$(buildEntity "Campaign")

$(deriveJSON defaultOptions ''Campaign)

instance CampaignRunner env => RunMessage env Campaign where
  runMessage = $(entityRunMessage "Campaign")

instance HasRecord env Campaign where
  hasRecord key = hasRecord key . campaignLog . toAttrs
  hasRecordSet key = hasRecordSet key . campaignLog . toAttrs
  hasRecordCount key = hasRecordCount key . campaignLog . toAttrs

instance HasSet CompletedScenarioId env Campaign where
  getSet = getSet . toAttrs

instance HasList CampaignStoryCard env Campaign where
  getList = getList . toAttrs

instance HasCampaignStoryCard env Campaign where
  getCampaignStoryCard def s = pure . fromJustNote "missing card" $ find
    ((== def) . toCardDef)
    cards
   where
    attrs = toAttrs s
    cards = concat . toList $ campaignStoryCards attrs

instance Entity Campaign where
  type EntityId Campaign = CampaignId
  type EntityAttrs Campaign = CampaignAttrs
  toId = toId . toAttrs
  toAttrs = $(entityF "Campaign" "toAttrs")

instance Named Campaign where
  toName = toName . toAttrs

allCampaigns :: HashMap CampaignId (Difficulty -> Campaign)
allCampaigns = mapFromList
  [ ("01", NightOfTheZealot' . nightOfTheZealot)
  , ("02", TheDunwichLegacy' . theDunwichLegacy)
  , ("03", ThePathToCarcosa' . thePathToCarcosa)
  , ("50", ReturnToNightOfTheZealot' . returnToNightOfTheZealot)
  ]

lookupCampaign :: CampaignId -> (Difficulty -> Campaign)
lookupCampaign cid =
  fromJustNote ("Unknown campaign: " <> show cid) $ lookup cid allCampaigns

difficultyOf :: Campaign -> Difficulty
difficultyOf = campaignDifficulty . toAttrs

chaosBagOf :: Campaign -> [TokenFace]
chaosBagOf = campaignChaosBag . toAttrs
