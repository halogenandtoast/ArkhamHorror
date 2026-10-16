module Arkham.Homebrew.AndThenThereWereNone.Key (module Arkham.Homebrew.AndThenThereWereNone.Key) where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude

data AndThenThereWereNoneKey
  = SoldierIslandWasAbandoned
  | TheSoldierIslandMurdersWereSolved
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey AndThenThereWereNoneKey where
  toCampaignLogKey = HomebrewCampaignLogKey . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> readMay (unpack t)
    _ -> Nothing
