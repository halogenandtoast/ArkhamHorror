module Arkham.Treachery.Cards.BloodthirstySpirits (bloodthirstySpirits) where

import Arkham.Ability
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BloodthirstySpirits = BloodthirstySpirits TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodthirstySpirits :: TreacheryCard BloodthirstySpirits
bloodthirstySpirits = treachery BloodthirstySpirits Cards.bloodthirstySpirits

instance HasModifiersFor BloodthirstySpirits where
  getModifiersFor (BloodthirstySpirits a) = do
    for_ a.inThreatAreaOf \iid -> do
      withLocationOf iid \loc -> do
        atHaunted <- matches loc HauntedLocation
        modifiedWhen_ a atHaunted iid [AdditionalCostToCommit iid $ ResolveEachHauntedAbility loc]

instance HasAbilities BloodthirstySpirits where
  getAbilities (BloodthirstySpirits a) =
    [skillTestAbility $ restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You]

instance RunMessage BloodthirstySpirits where
  runMessage msg t@(BloodthirstySpirits attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> BloodthirstySpirits <$> liftRunMessage msg attrs
