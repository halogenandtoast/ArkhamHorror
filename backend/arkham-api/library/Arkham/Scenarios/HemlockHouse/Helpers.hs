module Arkham.Scenarios.HemlockHouse.Helpers where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Classes.HasGame (HasGame)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Grid (Pos (..))
import Arkham.Location.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Token (Token (..), countTokens)
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "hemlockHouse" a

-- | Floor number for a location from its grid Y position.
-- 1st Floor (Parlor/Foyer/Dining) is Y=0 → floor 1.
-- The Cellar (Shapeless Cellar enemy-location) is at Y<0 → floor 0.
getFloorNumber :: (HasGame m, Tracing m) => LocationId -> m Int
getFloorNumber lid = do
  mPos <- field LocationPosition lid
  case mPos of
    Just (Pos _ y) | y >= 0 -> pure (y + 1)
    _ -> pure 0

-- | Number of seals on a location.
-- Seals are modeled as Resource tokens placed by the act/agenda's seal action
-- ("Place 1 resource on it, as a seal").
locationSealCount :: (HasGame m, Tracing m) => LocationId -> m Int
locationSealCount lid = do
  ts <- field LocationTokens lid
  pure $ countTokens Resource ts

locationIsUnsealed :: (HasGame m, Tracing m) => LocationId -> m Bool
locationIsUnsealed lid = (== 0) <$> locationSealCount lid
