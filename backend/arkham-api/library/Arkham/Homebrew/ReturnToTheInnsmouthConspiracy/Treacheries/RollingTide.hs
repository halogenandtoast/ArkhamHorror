module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.RollingTide (rollingTide) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (
  decreaseThisFloodLevel,
  increaseThisFloodLevel,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Ocean))
import Arkham.Treachery.Import.Lifted

newtype RollingTide = RollingTide TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rollingTide :: TreacheryCard RollingTide
rollingTide = treachery RollingTide Cards.rollingTide

instance RunMessage RollingTide where
  runMessage msg t@(RollingTide attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- A fully flooded location can always drop a level, so "that can have its
      -- flood level decreased" adds nothing beyond being fully flooded.
      candidates <-
        select
          $ NearestLocationTo iid (FullyFloodedLocation <> not_ (LocationWithTrait Ocean))
      if null candidates
        then gainSurge attrs
        else chooseTargetM iid candidates $ handleTarget iid attrs
      pure t
    HandleTargetChoice iid (isSource attrs -> True) (LocationTarget lid) -> do
      decreaseThisFloodLevel lid
      floodable <- select $ RevealedLocation <> connectedTo (be lid) <> CanHaveFloodLevelIncreased
      -- "increase the flood level of two revealed locations connected to that location"
      chooseNM iid 2 $ targets floodable increaseThisFloodLevel
      -- "If no or exactly one flood level was increased this way, Rolling Tide gains surge."
      when (length floodable < 2) $ gainSurge attrs
      pure t
    _ -> RollingTide <$> liftRunMessage msg attrs
