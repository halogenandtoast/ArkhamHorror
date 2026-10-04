{- | Campaign-log keys owned by The Symphony of Erich Zann.

The scenario is a side story, so these are written into whatever campaign log is
running (or the standalone one).
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.Key (module Arkham.Homebrew.TheSymphonyOfErichZann.Key) where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude

data TheSymphonyOfErichZannKey
  = -- | Act 3b. Worth a heal or an extra experience at Resolution 1.
    YouSavedAllTheMusicians
  | -- | Resolution 1
    AllIsQuietAtRueDAuseilForNow
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey TheSymphonyOfErichZannKey where
  toCampaignLogKey = HomebrewCampaignLogKey . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> readMay (unpack t)
    _ -> Nothing
