module Arkham.Scenarios.TheDevourerBelow.Helpers where

import Arkham.Campaigns.NightOfTheZealot.Helpers
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theDevourerBelow" a
