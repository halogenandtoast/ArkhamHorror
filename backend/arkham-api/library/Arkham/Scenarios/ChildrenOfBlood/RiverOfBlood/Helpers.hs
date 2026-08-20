module Arkham.Scenarios.ChildrenOfBlood.RiverOfBlood.Helpers where

import Arkham.Campaigns.ChildrenOfBlood.Helpers
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "riverOfBlood" a
