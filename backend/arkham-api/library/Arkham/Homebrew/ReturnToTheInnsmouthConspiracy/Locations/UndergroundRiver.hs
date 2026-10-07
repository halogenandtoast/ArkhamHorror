module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Locations.UndergroundRiver (undergroundRiver) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (increaseThisFloodLevel)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype UndergroundRiver = UndergroundRiver LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undergroundRiver :: LocationCard UndergroundRiver
undergroundRiver =
  locationWith UndergroundRiver Cards.undergroundRiver 6 (PerPlayer 1) connectsToAdjacent

{- | "Forced - When Underground River's flood level would be increased: If possible,
increase the flood level of the nearest location that can have its flood level
increased instead."

A real Forced ability on 'WouldIncreaseFloodLevel', which the engine raises for every
attempted increase whether or not the level can move. Deliberately no
'CannotBeFullyFlooded': that pins the river at its own maximum and
'CanHaveFloodLevelIncreased' then filters it out of the flood effects that pick targets
that way -- Rolling Tide, Rising Tides, most of them -- so nothing would aim an increase
at the river and this would never fire. (The official Underground River, 07104, is the
one whose text is "can not be fully flooded".)
-}
instance HasAbilities UndergroundRiver where
  getAbilities (UndergroundRiver a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ WouldIncreaseFloodLevel #when (be a)

instance RunMessage UndergroundRiver where
  runMessage msg l@(UndergroundRiver attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (wouldBecome -> Just level) _ -> do
      -- "the nearest location that can have its flood level increased": several can be
      -- equally near, and then the player resolving the Forced picks which one rises.
      nearest <-
        select $ NearestLocationToLocation attrs.id (CanHaveFloodLevelIncreased <> not_ (be attrs))
      if notNull nearest
        then do
          -- "instead": the river's own pending change is replaced, not added to.
          cancelTheIncrease
          chooseTargetM iid nearest increaseThisFloodLevel
        else
          -- Nowhere to send the water, so there is no "instead" to apply and the river
          -- takes the increase itself -- except that it can never be fully flooded, and
          -- that cap is absolute, so such an increase does nothing at all.
          when (level == FullyFlooded) cancelTheIncrease
      pure l
    _ -> UndergroundRiver <$> liftRunMessage msg attrs
   where
    cancelTheIncrease = allMatchingDon't \case
      Do (SetFloodLevel lid _) -> lid == attrs.id
      _ -> False

-- | The level the flood would be raised to, read off the window that triggered the Forced.
wouldBecome :: [Window] -> Maybe FloodLevel
wouldBecome ws = listToMaybe [raisedTo | (windowType -> Window.WouldIncreaseFloodLevel _ _ raisedTo) <- ws]
