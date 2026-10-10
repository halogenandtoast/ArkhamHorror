module Arkham.Homebrew.AgesUnwound.Treacheries.UpToSomething (upToSomething) where

import Arkham.Card
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (placeThisTask)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype UpToSomething = UpToSomething TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

upToSomething :: TreacheryCard UpToSomething
upToSomething = treachery UpToSomething Cards.upToSomething

{- | "__Revelation__ - Attach the set-aside Scheme of the Myriad to Rome. Search
the encounter deck and discard pile for 2 copies of The Myriad Gentleman and
spawn them in Rome. / __Task__ - Find whatever the Myriad is after in Rome."

Two searches rather than one for two cards: 'findEncounterCard' pulls a single
card, and the second search runs after the first copy has already left the deck,
so it cannot find the same one twice. The copy searched for is /One of Many/
(@:ages-unwound:220@) -- the only The Myriad Gentleman in Scenario V's encounter
deck, four of which come from the @myriad@ set.

/Harnessing a Tear in Reality/ completes this Task.
-}
instance RunMessage UpToSomething where
  runMessage msg t@(UpToSomething attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeThisTask attrs
      selectForMaybeM (locationIs Locations.rome)
        $ createTreacheryAt_ Cards.schemeOfTheMyriad
        . AttachedToLocation
      replicateM_ 2 $ findEncounterCard iid attrs Enemies.theMyriadGentleman_220
      pure t
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      createEnemy_ card (locationIs Locations.rome)
      pure t
    _ -> UpToSomething <$> liftRunMessage msg attrs
