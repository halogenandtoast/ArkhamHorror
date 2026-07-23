module Arkham.Scenarios.TheWesternWall.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Classes.HasGame
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theWesternWall" a

-- A location's vertical "level": row 0 is level 1 and distance from row 0
-- descends through levels 2–5. This also supports saves made with positive rows.
getLocationLevel
  :: (AsId investigator, IdOf investigator ~ InvestigatorId, HasGame m, Tracing m)
  => investigator -> m Int
getLocationLevel investigator =
  fromMaybe 0 <$> runMaybeT do
    loc <- MaybeT $ getMaybeLocation investigator
    pos <- MaybeT $ field LocationPosition loc
    pure (abs pos.row + 1)
