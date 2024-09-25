module Arkham.Scenarios.LostInTimeAndSpace.Helpers where

import Arkham.Campaigns.TheDunwichLegacy.Helpers
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "lostInTimeAndSpace" a
