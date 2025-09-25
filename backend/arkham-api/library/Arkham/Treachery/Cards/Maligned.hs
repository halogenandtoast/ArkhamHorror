module Arkham.Treachery.Cards.Maligned (maligned) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (IncreaseCostOf), inThreatAreaGets)
import Arkham.Matcher
import Arkham.Trait (Trait (Witch))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Maligned = Maligned TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maligned :: TreacheryCard Maligned
maligned = treachery Maligned Cards.maligned

instance HasModifiersFor Maligned where
  getModifiersFor (Maligned attrs) = inThreatAreaGets attrs [IncreaseCostOf #any 1]

instance HasAbilities Maligned where
  getAbilities (Maligned a) = [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]

instance RunMessage Maligned where
  runMessage msg t@(Maligned attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      whenAny (at_ (locationWithInvestigator iid) <> ExhaustedEnemy <> EnemyWithTrait Witch) do
        skillTestAutomaticallySucceeds (attrs.ability 2) sid
      beginSkillTest sid iid attrs iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Maligned <$> liftRunMessage msg attrs
