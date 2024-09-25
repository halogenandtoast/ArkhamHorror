module Arkham.Scenarios.DimCarcosa.Helpers (
  module Arkham.Helpers.Story,
  module Arkham.Scenarios.DimCarcosa.Helpers,
) where

import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Helpers.Story
import Arkham.I18n
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "dimCarcosa" a
