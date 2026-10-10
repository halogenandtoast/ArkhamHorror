module Arkham.Homebrew.AgesUnwound.Acts.JustOneMan (justOneMan) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Matcher

newtype JustOneMan = JustOneMan ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

justOneMan :: ActCard JustOneMan
justOneMan = act (2, A) JustOneMan Cards.justOneMan Nothing

{- | "Objective - At the end of the round, investigators at the Landing may spend
the requisite number of clues to advance."

The act's other printed ability -- "Forced - When a treachery card would search,
reveal or discard from the encounter deck to find an enemy: Instead spawn a copy
of The Myriad Gentleman, treating it as having been found by that treachery" --
has no window to hang on; see the @TODO(ages-unwound)@ in 'RunMessage'.
-}
instance HasAbilities JustOneMan where
  getAbilities (JustOneMan x) =
    [ mkAbility x 1
        $ Objective
        $ triggered (RoundEnds #when)
        $ GroupClueCost (PerPlayer 4) (locationIs Locations.landing)
    ]

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
instance RunMessage JustOneMan where
  runMessage msg a@(JustOneMan attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #clues attrs (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- "Reveal the Study."
      study <- selectJust $ locationIs Locations.study
      reveal study

      {- "Spawn the set-aside The Myriad Gentleman (Master of the House) and
      1[per_investigator] copies of The Myriad Gentleman (Thousandfold Man) at
      the Study." -}
      createSetAsideEnemy_ Enemies.theMyriadGentleman_043 study
      n <- perPlayer 1
      lead <- getLead
      spawnMyriadCopiesAt lead n study

      advanceActDeck attrs
      pure a
    _ -> JustOneMan <$> liftRunMessage msg attrs
