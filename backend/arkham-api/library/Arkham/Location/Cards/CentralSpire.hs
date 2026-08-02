module Arkham.Location.Cards.CentralSpire (centralSpire) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Window (getBatchId)

newtype CentralSpire = CentralSpire LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

centralSpire :: LocationCard CentralSpire
centralSpire = location CentralSpire Cards.centralSpire 3 (Static 2)

instance HasModifiersFor CentralSpire where
  getModifiersFor (CentralSpire a) = modifySelf a [CannotBeMoved, CannotLeavePlay]

instance HasAbilities CentralSpire where
  getAbilities (CentralSpire a) =
    if a.revealed
      then
        extendRevealed1 a
          $ groupLimit PerRound
          $ restricted a 1 Here
          $ actionAbilityWithCost (AtLeastOne (Fixed 3) (ResourceCost 1))
      else extendUnrevealed1 a (summitEntry a 9)

instance RunMessage CentralSpire where
  runMessage msg l@(CentralSpire attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 9 (getBatchId -> batchId) _ -> do
      summitEntryToll attrs 9 iid batchId
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 9 -> True) -> do
      summitEntryFailed attrs 9 iid
      pure l
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalResourcePayment -> n) -> do
      revealedLocations <- select RevealedLocation
      chooseTargetM iid revealedLocations \lid ->
        placeClues (attrs.ability 1) lid n
      pure l
    _ -> CentralSpire <$> liftRunMessage msg attrs
