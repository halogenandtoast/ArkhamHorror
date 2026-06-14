module Arkham.Scenarios.TheGrandVault.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.I18n
import Arkham.Id (LocationId)
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Source (Sourceable)
import Arkham.Tracing (Tracing)

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theGrandVault" a

-- | Per the scenario rules, an "activated" location is marked with a resource
-- token. Effects that reference activated/deactivated locations query this.
activatedLocation :: LocationMatcher
activatedLocation = LocationWithResources (atLeast 1)

getActivatedCount :: (HasGame m, Tracing m) => m Int
getActivatedCount = selectCount activatedLocation

-- | "Activate this location" — place a resource token on it (you cannot activate
-- a location that is already activated).
activateLocation :: (ReverseQueue m, Sourceable s) => s -> LocationId -> m ()
activateLocation s lid = do
  active <- lid <=~> activatedLocation
  unless active $ placeTokens s lid #resource 1

-- | "Deactivate this location" — remove its activation marker.
deactivateLocation :: (ReverseQueue m, Sourceable s) => s -> LocationId -> m ()
deactivateLocation s lid = removeTokens s lid #resource 1
