module Arkham.Treachery.Cards.DreamsOfRlyeh (DreamsOfRlyeh (..), dreamsOfRlyeh) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype DreamsOfRlyeh = DreamsOfRlyeh TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamsOfRlyeh :: TreacheryCard DreamsOfRlyeh
dreamsOfRlyeh = treachery DreamsOfRlyeh Cards.dreamsOfRlyeh

instance HasModifiersFor DreamsOfRlyeh where
  getModifiersFor (DreamsOfRlyeh attrs) =
    inThreatAreaGets attrs [SkillModifier #willpower (-1), SanityModifier (-1)]

instance HasAbilities DreamsOfRlyeh where
  getAbilities (DreamsOfRlyeh a) = [skillTestAbility $ restrictedAbility a 1 OnSameLocation actionAbility]

instance RunMessage DreamsOfRlyeh where
  runMessage msg t@(DreamsOfRlyeh attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> DreamsOfRlyeh <$> runMessage msg attrs
