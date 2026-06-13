module Arkham.Scenarios.OneLastJob.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "oneLastJob" a
