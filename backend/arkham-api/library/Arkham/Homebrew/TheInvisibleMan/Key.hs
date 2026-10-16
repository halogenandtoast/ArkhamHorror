module Arkham.Homebrew.TheInvisibleMan.Key (module Arkham.Homebrew.TheInvisibleMan.Key) where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude

data TheInvisibleManKey
  = TheInvisibleManEscapedJustice
  | TheInvisibleManMetHisEnd
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey TheInvisibleManKey where
  toCampaignLogKey = HomebrewCampaignLogKey . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> readMay (unpack t)
    _ -> Nothing
