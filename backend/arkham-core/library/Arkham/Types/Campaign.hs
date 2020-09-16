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
import Generics.SOP hiding (Generic)
import qualified Generics.SOP as SOP
import Safe (fromJustNote)

data Campaign
  = NightOfTheZealot' NightOfTheZealot
  | TheDunwichLegacy' TheDunwichLegacy
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, SOP.Generic)

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
campaignAttrs = getAttrs

getAttrs :: (All2 IsAttrs (Code a), SOP.Generic a) => a -> Attrs
getAttrs a = go (unSOP $ from a)
 where
  go :: (All2 IsAttrs xs) => NS (NP I) xs -> Attrs
  go (S next) = go next
  go (Z (I x :* _)) = coerce x
  go (Z Nil) = error "should not happen"

class (Coercible a Attrs) => IsAttrs a
instance (Coercible a Attrs) => IsAttrs a
