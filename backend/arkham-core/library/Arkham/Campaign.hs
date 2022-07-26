module Arkham.Campaign where

import Arkham.Prelude

import Arkham.Campaign.Campaigns
import Arkham.Campaign.Runner
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Id
import Arkham.Token
import Data.Typeable

data Campaign = forall a. IsCampaign a => Campaign a

instance Eq Campaign where
  (Campaign (a :: a)) == (Campaign (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Campaign where
  show (Campaign a) = show a

instance ToJSON Campaign where
  toJSON (Campaign a) = toJSON a

instance RunMessage Campaign where
  runMessage msg (Campaign a)= Campaign <$> runMessage msg a

instance Entity Campaign where
  type EntityId Campaign = CampaignId
  type EntityAttrs Campaign = CampaignAttrs
  toId = toId . toAttrs
  toAttrs (Campaign a) = toAttrs a
  overAttrs f (Campaign a) = Campaign $ overAttrs f a

lookupCampaign :: CampaignId -> (Difficulty -> Campaign)
lookupCampaign cid =
  fromJustNote ("Unknown campaign: " <> show cid) $ lookup cid allCampaigns

difficultyOf :: Campaign -> Difficulty
difficultyOf = campaignDifficulty . toAttrs

chaosBagOf :: Campaign -> [TokenFace]
chaosBagOf = campaignChaosBag . toAttrs

instance FromJSON Campaign where
  parseJSON v = flip (withObject "Campaign") v $ \o -> do
    cCode :: Text <- o .: "id"
    case cCode of
      "01" -> Campaign . NightOfTheZealot <$> parseJSON v
      "02" -> Campaign . TheDunwichLegacy <$> parseJSON v
      "03" -> Campaign . ThePathToCarcosa <$> parseJSON v
      "04" -> Campaign . TheForgottenAge <$> parseJSON v
      "50" -> Campaign . ReturnToNightOfTheZealot <$> parseJSON v
      _ -> error "invalid campaign"

allCampaigns :: HashMap CampaignId (Difficulty -> Campaign)
allCampaigns = mapFromList
  [ ("01", Campaign <$> nightOfTheZealot)
  , ("02", Campaign <$> theDunwichLegacy)
  , ("03", Campaign <$> thePathToCarcosa)
  , ("04", Campaign <$> theForgottenAge)
  , ("50", Campaign <$> returnToNightOfTheZealot)
  ]
