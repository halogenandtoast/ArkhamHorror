module Arkham.Scenarios.TheUnspeakableOath.Helpers where

import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theUnspeakableOath" a
