module Arkham.Homebrew.CircusExMortis.Locations.ForestChasm (forestChasm) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.CircusExMortis.Helpers (destinyLocationOvercome, investigatorWithDestiny)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

{- | The back of the Raise the Torch Destiny story (:203b). 'otherSideIs' makes the def
single-sided, so it is revealed the instant it is created and its 8 fixed clues are
placed by its own 'PlacedLocation'.
-}
newtype ForestChasm = ForestChasm LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forestChasm :: LocationCard ForestChasm
forestChasm = location ForestChasm Cards.forestChasm 3 (Static 8)

{- | "The last clue on Forest Chasm cannot be discovered or moved by investigators whose
destiny is not \"torch.\"" One clue left is the last one, so both bans only exist then.

The discovery half is exact: 'CannotDiscoverCluesAt' on the seats that may not take it
feeds 'getCanDiscoverClues', so no ability is even offered to them.

The movement half is not. Clue movement is gated by 'CannotMoveCluesFromHere', which sits
on the LOCATION and carries no investigator scope (its only two readers are Gene
Beauregard (3) and Vantage Point), so this also stops the "torch" seat from moving the
last clue away -- a legal play the card does allow. Scoping it exactly needs an
investigator-side modifier the engine does not have; the error is in the safe direction,
since the point of the sentence is to stop anyone else emptying the chasm.
-}
instance HasModifiersFor ForestChasm where
  getModifiersFor (ForestChasm a) = when (a.clues == 1) do
    torch <- investigatorWithDestiny "torch"
    modifySelect a (not_ torch) [CannotDiscoverCluesAt (be a)]
    modifySelf a [CannotMoveCluesFromHere]

{- | "Forced - If there are no clues on Forest Chasm: Move each investigator and enemy on
it to a connecting location, flip it, and move it to the victory display."

Hung off 'LastClueRemovedFromLocation' rather than 'AnyWindow'. The clues are placed by
'PlacedLocation' *behind* the windows it opens for entering play, so an 'AnyWindow' Forced
reading "no clues on this" would fire in the enters-play window and destroy the location
before its 8 clues ever arrived.
-}
instance HasAbilities ForestChasm where
  getAbilities (ForestChasm a) =
    extendRevealed1 a
      $ onlyOnce
      $ restricted a 1 (thisExists a LocationWithoutClues)
      $ forced
      $ LastClueRemovedFromLocation #after (be a)

instance RunMessage ForestChasm where
  runMessage msg l@(ForestChasm attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      destinyLocationOvercome (attrs.ability 1) iid Stories.raiseTheTorch attrs
      pure l
    _ -> ForestChasm <$> liftRunMessage msg attrs
