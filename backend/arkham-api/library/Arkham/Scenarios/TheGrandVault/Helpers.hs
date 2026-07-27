module Arkham.Scenarios.TheGrandVault.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.I18n
import Arkham.Id (LocationId)
import Arkham.Location.Grid (Pos (..))
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Source (Sourceable)
import Arkham.Tracing (Tracing)

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theGrandVault" a

{- | The vault layout from the location placement diagram. The Great Stair anchors
the far left and the Sealed Chamber the far right, with the seven shuffled Vault
Chambers filling every other space:

@
   .      VC        VC    VC   .
 Stair  Platform  Core    VC  Sealed
   .      VC        VC    VC   .
@

Listed left-to-right, top-to-bottom, which is the order setup deals the shuffled
Vault Chambers into.
-}
vaultChamberPositions :: [Pos]
vaultChamberPositions =
  [ Pos (-1) 1
  , Pos 0 1
  , Pos 1 1
  , Pos 1 0
  , Pos (-1) (-1)
  , Pos 0 (-1)
  , Pos 1 (-1)
  ]

-- | The bottom left Vault Chamber, activated during setup in every game.
vaultBottomLeft :: Pos
vaultBottomLeft = Pos (-1) (-1)

{- | Bottom left, bottom right, and top right — the three Vault Chambers activated
during setup when the power was /not/ diverted.
-}
vaultPreActivatedPositions :: [Pos]
vaultPreActivatedPositions = [vaultBottomLeft, Pos 1 (-1), Pos 1 1]

-- | The three Vault Chambers in the bottom row, which begin partially flooded.
vaultBottomRow :: [Pos]
vaultBottomRow = [Pos (-1) (-1), Pos 0 (-1), Pos 1 (-1)]

{- | Per the scenario rules, an "activated" location is marked with a resource
token. Effects that reference activated/deactivated locations query this.
-}
activatedLocation :: LocationMatcher
activatedLocation = LocationWithResources (atLeast 1)

getActivatedCount :: (HasGame m, Tracing m) => m Int
getActivatedCount = selectCount activatedLocation

{- | "Activate this location" — place a resource token on it (you cannot activate
a location that is already activated).
-}
activateLocation :: (ReverseQueue m, Sourceable s) => s -> LocationId -> m ()
activateLocation s lid = do
  active <- lid <=~> activatedLocation
  unless active $ placeTokens s lid #resource 1

-- | "Deactivate this location" — remove its activation marker.
deactivateLocation :: (ReverseQueue m, Sourceable s) => s -> LocationId -> m ()
deactivateLocation s lid = removeTokens s lid #resource 1
