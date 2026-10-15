module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Key (module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Key) where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude

data TheFallOfTheHouseOfUsherKey
  = TheHouseOfUsherHasFallen
  | TheInvestigatorsAvengedTheHouseOfUsher
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey TheFallOfTheHouseOfUsherKey where
  toCampaignLogKey = HomebrewCampaignLogKey . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> readMay (unpack t)
    _ -> Nothing
