module Arkham.Scenarios.CourtOfTheAncients.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Card (cardMatch)
import Arkham.Classes.HasGame
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Scenario (getVictoryDisplay)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait (Trait (Glyph))
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "courtOfTheAncients" a

-- | The tower is a vertical grid: a location's "level" is its grid row + 1
-- (row 0 = level 1). Mirrors ToTheForbiddenPeaks/TheWesternWall: must read the
-- level via the investigator so the @Projection Location@ instance is visible.
getLocationLevel
  :: (AsId investigator, IdOf investigator ~ InvestigatorId, HasGame m, Tracing m)
  => investigator -> m Int
getLocationLevel investigator = fromMaybe 0 <$> runMaybeT do
  loc <- MaybeT $ getMaybeLocation investigator
  pos <- MaybeT $ field LocationPosition loc
  pure (pos.row + 1)

-- | The number of [[Glyph]] cards currently in the victory display (drives the
-- skull token and several Court effects).
getVictoryGlyphCount :: (HasGame m, Tracing m) => m Int
getVictoryGlyphCount = count (`cardMatch` CardWithTrait Glyph) <$> getVictoryDisplay
