{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Campaign where

import Arkham.Prelude

import Arkham.Campaign.Campaigns
import Arkham.Campaign.Runner
import Arkham.Campaigns.TheDreamEaters.Meta qualified as TheDreamEaters
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Id
import Control.Monad.Fail
import GHC.Records

instance RunMessage Campaign where
  runMessage msg (Campaign a) = Campaign <$> runMessage msg a

lookupCampaign :: CampaignId -> Difficulty -> Campaign
lookupCampaign cid = case lookup cid allCampaigns of
  Nothing -> error $ "Unknown campaign: " <> show cid
  Just (SomeCampaign f) -> Campaign . f

instance HasField "currentCampaignMode" Campaign (Maybe TheDreamEaters.CampaignPart) where
  getField (Campaign c) = (toAttrs c).currentCampaignMode

instance HasField "currentCampaignMode" CampaignAttrs (Maybe TheDreamEaters.CampaignPart) where
  getField c = case maybeResult @TheDreamEaters.Metadata (campaignMeta c) of
    Nothing -> Nothing
    Just x -> x.currentCampaignMode

instance FromJSON Campaign where
  parseJSON = withObject "Campaign" $ \o -> do
    cCode <- o .: "id"
    case lookup cCode allCampaigns of
      Nothing -> fail $ "Unknown campaign: " <> show cCode
      Just (SomeCampaign (_ :: Difficulty -> a)) ->
        Campaign <$> parseJSON @a (Object o)

data SomeCampaign = forall a. IsCampaign a => SomeCampaign (Difficulty -> a)

allCampaigns :: Map CampaignId SomeCampaign
allCampaigns =
  mapFromList
    [ ("01", SomeCampaign nightOfTheZealot)
    , ("02", SomeCampaign theDunwichLegacy)
    , ("03", SomeCampaign thePathToCarcosa)
    , ("04", SomeCampaign theForgottenAge)
    , ("05", SomeCampaign theCircleUndone)
    , ("06", SomeCampaign theDreamEaters)
    , ("07", SomeCampaign theInnsmouthConspiracy)
    , ("08", SomeCampaign edgeOfTheEarth)
    , ("50", SomeCampaign returnToNightOfTheZealot)
    ]
