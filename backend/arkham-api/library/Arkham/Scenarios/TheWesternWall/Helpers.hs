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

-- A location's vertical "level": its grid row + 1 (row 0 is level 1). Used by the
-- scenario's chaos tokens ("X is your location's level").
getLocationLevel
  :: (AsId investigator, IdOf investigator ~ InvestigatorId, HasGame m, Tracing m)
  => investigator -> m Int
getLocationLevel investigator =
  fromMaybe 0 <$> runMaybeT do
    loc <- MaybeT $ getMaybeLocation investigator
    pos <- MaybeT $ field LocationPosition loc
    pure (pos.row + 1)
