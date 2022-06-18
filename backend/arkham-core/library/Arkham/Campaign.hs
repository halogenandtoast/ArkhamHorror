{-# LANGUAGE TemplateHaskell #-}
module Arkham.Campaign where

import Arkham.Prelude

import Arkham.Campaign.Campaigns
import Arkham.Campaign.Runner
import Arkham.Classes
import Arkham.Classes.Entity.TH
import Arkham.Difficulty
import Arkham.Id
import Arkham.Token
import Data.Aeson.TH

$(buildEntity "Campaign")
$(deriveJSON defaultOptions ''Campaign)

instance RunMessage Campaign where
  runMessage = $(entityRunMessage "Campaign")

instance Entity Campaign where
  type EntityId Campaign = CampaignId
  type EntityAttrs Campaign = CampaignAttrs
  toId = toId . toAttrs
  toAttrs = $(entityF "Campaign" "toAttrs")

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
