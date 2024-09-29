module Arkham.Treachery.Cards.Inundated (inundated, Inundated (..)) where

import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (getModifiers)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Scenarios.InTooDeep.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Inundated = Inundated TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inundated :: TreacheryCard Inundated
inundated = treachery Inundated Cards.inundated

instance RunMessage Inundated where
  runMessage msg t@(Inundated attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      ref <- newIORef (0 :: Int)
      getLocationOf iid >>= traverse_ \lid -> do
        unlessM (lid <=~> locationIs Locations.desolateCoastline) do
          getConnectedLocations lid >>= traverse_ \lid' -> do
            whenM (not <$> (lid' <=~> locationIs Locations.desolateCoastline)) do
              mods <- getModifiers lid'
              let ls = concat [xs | Barricades xs <- mods]
              when (lid `notElem` ls) do
                modifyIORef ref (+ 1)
                placeBarrier lid lid'
      placed <- readIORef ref
      when (placed == 0) $ gainSurge attrs
      pure t
    _ -> Inundated <$> liftRunMessage msg attrs
