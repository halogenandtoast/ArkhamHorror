module Arkham.Treachery.Cards.DreamsOfRlyeh (dreamsOfRlyeh) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DreamsOfRlyeh = DreamsOfRlyeh TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamsOfRlyeh :: TreacheryCard DreamsOfRlyeh
dreamsOfRlyeh = treachery DreamsOfRlyeh Cards.dreamsOfRlyeh

instance HasModifiersFor DreamsOfRlyeh where
  getModifiersFor (DreamsOfRlyeh attrs) =
    inThreatAreaGets attrs [SkillModifier #willpower (-1), SanityModifier (-1)]

instance HasAbilities DreamsOfRlyeh where
  getAbilities (DreamsOfRlyeh a) = [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]

instance RunMessage DreamsOfRlyeh where
  runMessage msg t@(DreamsOfRlyeh attrs) = runQueueT $ case msg of
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
    _ -> DreamsOfRlyeh <$> liftRunMessage msg attrs
