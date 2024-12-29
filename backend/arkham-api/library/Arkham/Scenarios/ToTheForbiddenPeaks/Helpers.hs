module Arkham.Scenarios.ToTheForbiddenPeaks.Helpers where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.I18n
import Arkham.Location.Types (LocationAttrs)
import Arkham.Matcher.Base
import Arkham.Matcher.Location
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "toTheForbiddenPeaks" a

below :: LocationAttrs -> LocationMatcher
below a = LocationInRow (maybe 0 (subtract 1) a.row)

inRow :: LocationAttrs -> Int -> LocationMatcher
inRow a n = be a <> LocationInRow n
