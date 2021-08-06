{-# LANGUAGE TemplateHaskell #-}
module Arkham.Types.Campaign where

import Arkham.Prelude

import Arkham.Types.Campaign.Attrs
import Arkham.Types.Campaign.Campaigns
import Arkham.Types.Campaign.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Id
import Arkham.Types.Name
import Arkham.Types.Token

$(buildEntity "Campaign")

instance CampaignRunner env => RunMessage env Campaign where
  runMessage = genericRunMessage

instance HasRecord Campaign where
  hasRecord key = ask >>= runReaderT (hasRecord key) . campaignLog . toAttrs
  hasRecordSet key =
    ask >>= runReaderT (hasRecordSet key) . campaignLog . toAttrs
  hasRecordCount key =
    ask >>= runReaderT (hasRecordCount key) . campaignLog . toAttrs

instance HasSet CompletedScenarioId env Campaign where
  getSet = getSet . toAttrs

instance HasList CampaignStoryCard env Campaign where
  getList = getList . toAttrs

instance Entity Campaign where
  type EntityId Campaign = CampaignId
  type EntityAttrs Campaign = CampaignAttrs

instance Named Campaign where
  toName = toName . toAttrs

allCampaigns :: Map CampaignId (Difficulty -> Campaign)
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

chaosBagOf :: Campaign -> [TokenFace]
chaosBagOf = campaignChaosBag . toAttrs
