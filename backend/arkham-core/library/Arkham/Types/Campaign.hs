{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Campaign where

import Arkham.Json
import Arkham.Types.Campaign.Attrs
import Arkham.Types.Campaign.Campaigns.NightOfTheZealot
import Arkham.Types.Campaign.Campaigns.TheDunwichLegacy
import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignId
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Token
import ClassyPrelude
import Data.Coerce
import Safe (fromJustNote)

data Campaign
  = NightOfTheZealot' NightOfTheZealot
  | TheDunwichLegacy' TheDunwichLegacy
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance (CampaignRunner env) => RunMessage env Campaign

instance HasRecord Campaign where
  hasRecord key = hasRecord key . campaignLog . campaignAttrs
  hasRecordSet key = hasRecordSet key . campaignLog . campaignAttrs

allCampaigns :: HashMap CampaignId (Difficulty -> Campaign)
allCampaigns = mapFromList
  [ ("01", NightOfTheZealot' . nightOfTheZealot)
  , ("02", TheDunwichLegacy' . theDunwichLegacy)
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
