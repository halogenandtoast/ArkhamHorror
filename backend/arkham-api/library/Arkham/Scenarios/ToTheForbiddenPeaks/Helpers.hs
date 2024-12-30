module Arkham.Scenarios.ToTheForbiddenPeaks.Helpers where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Classes.HasGame
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Types (Field (..), LocationAttrs)
import Arkham.Matcher.Base
import Arkham.Matcher.Location
import Arkham.Prelude
import Arkham.Projection

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "toTheForbiddenPeaks" a

below :: LocationAttrs -> LocationMatcher
below a = LocationInRow (maybe 0 (subtract 1) a.row)

inRow :: LocationAttrs -> Int -> LocationMatcher
inRow a n = be a <> LocationInRow n

getLevel
  :: (AsId investigator, IdOf investigator ~ InvestigatorId, HasGame m) => investigator -> m Int
getLevel investigator =
  fromMaybe 0 <$> runMaybeT do
    loc <- MaybeT $ getMaybeLocation investigator
    pos <- MaybeT $ field LocationPosition loc
    pure pos.row
