module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Locations.UndergroundRiver (undergroundRiver) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (getFloodLevel, increaseThisFloodLevel)
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype UndergroundRiver = UndergroundRiver LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undergroundRiver :: LocationCard UndergroundRiver
undergroundRiver =
  locationWith UndergroundRiver Cards.undergroundRiver 6 (PerPlayer 1) connectsToAdjacent

-- | "Underground River can not be fully flooded."
instance HasModifiersFor UndergroundRiver where
  getModifiersFor (UndergroundRiver a) = whenRevealed a $ modifySelf a [CannotBeFullyFlooded]

{- | "Forced - When Underground River's flood level would be increased: If possible,
increase the flood level of the nearest location that can have its flood level
increased instead."

Handled by intercepting the location's own 'IncreaseFloodLevel' rather than through
a window, because 'CannotBeFullyFlooded' clamps the new level to the current one and
'SetFloodLevel' then raises no window at all. (Shrouded Cistern swallows
'DecreaseFloodLevel' the same way.) Only the increase that would have fully flooded
it is redirected; the first one, from unflooded to partially flooded, is allowed.
-}
instance RunMessage UndergroundRiver where
  runMessage msg l@(UndergroundRiver attrs) = runQueueT $ case msg of
    IncreaseFloodLevel lid | lid == attrs.id -> do
      getFloodLevel attrs >>= \case
        PartiallyFlooded -> do
          nearest <-
            select $ NearestLocationToLocation attrs.id (CanHaveFloodLevelIncreased <> not_ (be attrs))
          for_ (take 1 nearest) increaseThisFloodLevel
          pure l
        _ -> UndergroundRiver <$> liftRunMessage msg attrs
    _ -> UndergroundRiver <$> liftRunMessage msg attrs
