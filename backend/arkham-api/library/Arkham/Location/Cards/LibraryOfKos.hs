module Arkham.Location.Cards.LibraryOfKos (libraryOfKos) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Helpers.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheHeartOfMadness.Helpers

newtype LibraryOfKos = LibraryOfKos LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

libraryOfKos :: LocationCard LibraryOfKos
libraryOfKos = location LibraryOfKos Cards.libraryOfKos 5 (PerPlayer 1)

instance HasAbilities LibraryOfKos where
  getAbilities (LibraryOfKos a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 (youExist (InvestigatorWithDormantSeal SealE)) actionAbility

instance RunMessage LibraryOfKos where
  runMessage msg l@(LibraryOfKos attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      targetAmount <- perPlayer 1
      iids <- select $ investigatorAt attrs
      sameRing <- getLocationsOnSameRing attrs.label UnrevealedLocation
      chooseOneM iid do
        labeled "Spend 1 {perPlayer} clues as a group to activate the seal" do
          push $ SpendClues targetAmount iids
          activateSeal SealE
          chooseOneAtATimeM iid $ targets sameRing $ lookAtRevealed iid (attrs.ability 1)
        labeled "Do not spend clues" nothing

      pure l
    _ -> LibraryOfKos <$> liftRunMessage msg attrs
