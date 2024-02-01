{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Campaign where

import Arkham.Prelude

import Arkham.Campaign.Campaigns
import Arkham.Campaign.Runner
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Id

instance RunMessage Campaign where
  runMessage msg (Campaign a) = do
    !result <- runMessage msg a
    mThunk <- liftIO $ noThunks [show msg] result
    case mThunk of
      Nothing -> pure $ Campaign result
      Just thunk -> error $ "Thunks found in Campaign: " <> show thunk

lookupCampaign :: CampaignId -> Difficulty -> Campaign
lookupCampaign cid = case lookup cid allCampaigns of
  Nothing -> error $ "Unknown campaign: " <> show cid
  Just (SomeCampaign f) -> Campaign . f

instance FromJSON Campaign where
  parseJSON = withObject "Campaign" $ \o -> do
    cCode <- o .: "id"
    case lookup cCode allCampaigns of
      Nothing -> error $ "Unknown campaign: " <> show cCode
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
    , ("50", SomeCampaign returnToNightOfTheZealot)
    ]
