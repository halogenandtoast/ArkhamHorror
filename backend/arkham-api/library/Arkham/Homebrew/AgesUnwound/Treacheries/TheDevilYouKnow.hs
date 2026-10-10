module Arkham.Homebrew.AgesUnwound.Treacheries.TheDevilYouKnow (theDevilYouKnow) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (placeThisTask)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype TheDevilYouKnow = TheDevilYouKnow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDevilYouKnow :: TreacheryCard TheDevilYouKnow
theDevilYouKnow = treachery TheDevilYouKnow Cards.theDevilYouKnow

{- | "__Revelation__ - Attach the set-aside Contacting the Lodge treachery to
Arkham, Massachusetts. / __Task__ - Win the favor of the Silver Twilight Lodge."

/Favors for Favors/ completes this Task.
-}
instance RunMessage TheDevilYouKnow where
  runMessage msg t@(TheDevilYouKnow attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeThisTask attrs
      selectForMaybeM (locationIs Locations.arkhamMassachusetts_111)
        $ createTreacheryAt_ Cards.contactingTheLodge
        . AttachedToLocation
      pure t
    _ -> TheDevilYouKnow <$> liftRunMessage msg attrs
