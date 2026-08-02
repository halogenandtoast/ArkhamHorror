module Arkham.Location.Cards.FloatingSpire (floatingSpire) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Window (getBatchId)

newtype FloatingSpire = FloatingSpire LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

floatingSpire :: LocationCard FloatingSpire
floatingSpire = location FloatingSpire Cards.floatingSpire 4 (Static 1)

instance HasModifiersFor FloatingSpire where
  getModifiersFor (FloatingSpire a) = modifySelf a [CannotBeMoved, CannotLeavePlay]

instance HasAbilities FloatingSpire where
  getAbilities (FloatingSpire a) =
    if a.revealed
      then
        extendRevealed1 a
          $ groupLimit PerRound
          $ restricted a 1 Here
          $ actionAbilityWithCost (AtLeastOne (Fixed 3) (ResourceCost 1))
      else extendUnrevealed1 a (summitEntry a 9)

instance RunMessage FloatingSpire where
  runMessage msg l@(FloatingSpire attrs) = runQueueT $ case msg of
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
    _ -> FloatingSpire <$> liftRunMessage msg attrs
