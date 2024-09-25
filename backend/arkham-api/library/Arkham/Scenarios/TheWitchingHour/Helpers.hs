module Arkham.Scenarios.TheWitchingHour.Helpers where

import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theWitchingHour" a
