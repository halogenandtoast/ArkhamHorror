module Arkham.Scenarios.TheDrownedCity.TheDrownedQuarter.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theDrownedQuarter" a

{- | Why Undersea Parasite was flipped to its story back. Both of its forced
abilities flip the same card, but the back's text branches on which one did, so
the enemy stores this in its meta for the story side to read.
-}
data UnderseaParasiteFlip = FlippedByAttack | FlippedByLeavingPlay
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
