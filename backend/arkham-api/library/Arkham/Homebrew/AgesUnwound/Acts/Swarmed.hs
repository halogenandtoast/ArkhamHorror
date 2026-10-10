module Arkham.Homebrew.AgesUnwound.Acts.Swarmed (swarmed) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Matcher

newtype Swarmed = Swarmed ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | "Objective - Investigators in the Study may spend the requisite number of
clues to advance." No timing restriction, so it is the act's standing
advancement cost rather than an end-of-round trigger.

As on /Just One Man/, the "Forced - When a treachery card would search, reveal or
discard from the encounter deck to find an enemy" clause has no window to hang
on; see the @TODO(ages-unwound)@ note there.
-}
swarmed :: ActCard Swarmed
swarmed =
  act
    (3, A)
    Swarmed
    Cards.swarmed
    (Just $ GroupClueCost (PerPlayer 2) (locationIs Locations.study))

{- TODO(ages-unwound): "Forced - When a treachery card would search, reveal or
discard from the encounter deck to find an enemy: Instead spawn a copy of The
Myriad Gentleman, treating it as having been found by that treachery."

There is no window for "a treachery would search the encounter deck for an
enemy", and the find is pushed by the treachery itself (@FindEncounterCard@ /
@search@, whichever that card chose), so an act cannot intercept it without
either a new timing point or a modifier the drawing card consults. The only card
in the sets this scenario gathers that triggers it is /Out of Phase/
(@shifting_reality@), which is owned by another module. Needs a shared seam --
reported to the orchestrator rather than hand-rolled as a brittle queue scan
that would silently cover nothing. -}
instance RunMessage Swarmed where
  runMessage msg a@(Swarmed attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> Swarmed <$> liftRunMessage msg attrs
