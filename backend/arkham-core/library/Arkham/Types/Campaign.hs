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
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import Safe (fromJustNote)

allCampaigns :: HashMap CampaignId (Difficulty -> Campaign)
allCampaigns = HashMap.fromList
  [ ("01", NightOfTheZealot' . nightOfTheZealot)
  , ("02", TheDunwichLegacy' . theDunwichLegacy)
  ]

campaignAttrs :: Campaign -> Attrs
campaignAttrs = \case
  NightOfTheZealot' attrs -> coerce attrs
  TheDunwichLegacy' attrs -> coerce attrs

data Campaign
  = NightOfTheZealot' NightOfTheZealot
  | TheDunwichLegacy' TheDunwichLegacy
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (CampaignRunner env) => RunMessage env Campaign where
  runMessage msg = \case
    NightOfTheZealot' x -> NightOfTheZealot' <$> runMessage msg x
    TheDunwichLegacy' x -> TheDunwichLegacy' <$> runMessage msg x

lookupCampaign :: CampaignId -> (Difficulty -> Campaign)
lookupCampaign cid =
  fromJustNote ("Unknown campaign: " <> show cid)
    $ HashMap.lookup cid allCampaigns
