module Arkham.Homebrew.AgesUnwound.Treacheries.StrangePortal (strangePortal) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (placeThisTask)
import Arkham.Treachery.Import.Lifted

newtype StrangePortal = StrangePortal TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangePortal :: TreacheryCard StrangePortal
strangePortal = treachery StrangePortal Cards.strangePortal

{- | "__Revelation__ - Put the set-aside Another Realm into play. / __Task__ -
Explore the strange portal."

/Destination/, Another Realm's back, completes this Task.
-}
instance RunMessage StrangePortal where
  runMessage msg t@(StrangePortal attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeThisTask attrs
      placeSetAsideLocation_ Locations.anotherRealm
      pure t
    _ -> StrangePortal <$> liftRunMessage msg attrs
