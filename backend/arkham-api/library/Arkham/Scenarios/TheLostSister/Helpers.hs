module Arkham.Scenarios.TheLostSister.Helpers where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theLostSister" a
