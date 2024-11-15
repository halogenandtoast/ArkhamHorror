module Arkham.Scenarios.IceAndDeath.Helpers where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "iceAndDeath" a
