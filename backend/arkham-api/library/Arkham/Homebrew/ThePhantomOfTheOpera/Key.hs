module Arkham.Homebrew.ThePhantomOfTheOpera.Key (module Arkham.Homebrew.ThePhantomOfTheOpera.Key) where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude

data ThePhantomOfTheOperaKey
  = ThePhantomEscapedJustice
  | ThePhantomDestroyedHimself
  | ThePhantomHasFallen
  | ErikLivesOnBeneathParis
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey ThePhantomOfTheOperaKey where
  toCampaignLogKey = HomebrewCampaignLogKey . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> readMay (unpack t)
    _ -> Nothing
