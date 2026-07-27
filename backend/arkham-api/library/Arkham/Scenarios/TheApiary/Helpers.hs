module Arkham.Scenarios.TheApiary.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Classes.HasGame
import Arkham.Direction
import Arkham.Helpers.Scenario (getScenarioMeta)
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid (Pos (..))
import Arkham.Prelude
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theApiary" a

{- | Where each Apiary location sits on the map.

Movement is driven by the locations' own connection symbols, so the grid is
purely about placement: it fixes the picture, and it gives the Central Chamber
the four neighbours its rotation picks between.

Apiary Entrance is the hub. Growing Fields and the Hidden Vault it reveals run
up out of it, the two Fleshy Paths sit either side, and the warren descends
below it into the ring. The Enclave locations (western expedition) and the Nest
locations (eastern expedition) share connection symbols and only one set is ever
in play, so each pair shares a cell. The Central Chamber is a Nest card, so the
middle of the ring stays empty on the western map.

The ring is a cycle in the connection symbols themselves — Circle to Triangle to
Square to Heart and back — so consecutive ring locations sit diagonally from one
another and their connections draw as a diamond around the Central Chamber.

@
                              Hidden Vault
                             Growing Fields
        Fleshy Paths (W)     Apiary Entrance     Fleshy Paths (E)
                        Luminous Tunnels / Grasping Corridor
   Corrupted Vault   Central Chamber   Spawning Grounds / Starving Corridor   Churning Chasm
                          Lost Campsite / Acidic Coelom
@
-}
apiaryPositions :: [(CardDef, Pos)]
apiaryPositions =
  [ (Locations.apiaryEntranceBeckoningLight, Pos 0 0)
  , (Locations.apiaryEntranceDangerousExit, Pos 0 0)
  , (Locations.growingFields, Pos 0 1)
  , (Locations.hiddenVault, Pos 0 2)
  , (Locations.fleshyPathsWesternBurrows, Pos (-1) 0)
  , (Locations.fleshyPathsEasternBurrows, Pos 1 0)
  , (Locations.luminousTunnels, Pos 0 (-1))
  , (Locations.graspingCorridor, Pos 0 (-1))
  , (Locations.corruptedVault, Pos (-1) (-2))
  , (Locations.centralChamber, Pos 0 (-2))
  , (Locations.spawningGrounds, Pos 1 (-2))
  , (Locations.starvingCorridor, Pos 1 (-2))
  , (Locations.churningChasm, Pos 2 (-2))
  , (Locations.lostCampsite, Pos 1 (-1))
  , (Locations.acidicCoelom, Pos 1 (-1))
  ]

apiaryPosition :: HasCardCode a => a -> Maybe Pos
apiaryPosition (toCardCode -> cardCode) =
  lookup cardCode [(toCardCode def, pos) | (def, pos) <- apiaryPositions]

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
