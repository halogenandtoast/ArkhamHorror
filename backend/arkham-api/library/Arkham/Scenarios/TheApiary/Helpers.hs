module Arkham.Scenarios.TheApiary.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Classes.HasGame
import Arkham.Direction
import Arkham.Helpers.Scenario (getScenarioMeta)
import Arkham.I18n
import Arkham.Prelude
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theApiary" a

{- | The Central Chamber sits in the centre of a 4-location ring and is connected
only to the location it currently "faces" (the one beneath its bottom edge).
We store that facing as a GridDirection in the scenario meta; it starts facing
"down" (the location beneath it) and rotation steps it clockwise/counter.
-}
newtype ApiaryMeta = ApiaryMeta {centralChamberFacing :: GridDirection}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

initApiaryMeta :: ApiaryMeta
initApiaryMeta = ApiaryMeta GridDown

getCentralChamberFacing :: (Tracing m, HasGame m) => m GridDirection
getCentralChamberFacing = maybe GridDown centralChamberFacing <$> getScenarioMeta

-- Rotating the card clockwise/counter-clockwise moves which ring location is
-- "beneath" it. Clockwise: Down -> Left -> Up -> Right -> Down.
rotateFacingClockwise :: GridDirection -> GridDirection
rotateFacingClockwise = \case
  GridDown -> GridLeft
  GridLeft -> GridUp
  GridUp -> GridRight
  GridRight -> GridDown

rotateFacingCounterClockwise :: GridDirection -> GridDirection
rotateFacingCounterClockwise = \case
  GridDown -> GridRight
  GridRight -> GridUp
  GridUp -> GridLeft
  GridLeft -> GridDown

-- Degrees for the UI rotation modifier (0 = facing down / un-rotated).
facingDegrees :: GridDirection -> Int
facingDegrees = \case
  GridDown -> 0
  GridLeft -> 90
  GridUp -> 180
  GridRight -> 270
