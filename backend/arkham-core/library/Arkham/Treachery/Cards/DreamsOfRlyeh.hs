module Arkham.Treachery.Cards.DreamsOfRlyeh (
  DreamsOfRlyeh (..),
  dreamsOfRlyeh,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype DreamsOfRlyeh = DreamsOfRlyeh TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamsOfRlyeh :: TreacheryCard DreamsOfRlyeh
dreamsOfRlyeh = treachery DreamsOfRlyeh Cards.dreamsOfRlyeh

instance HasModifiersFor DreamsOfRlyeh where
  getModifiersFor (InvestigatorTarget iid) (DreamsOfRlyeh attrs) =
    pure
      $ toModifiers attrs
      $ guard (treacheryOnInvestigator iid attrs)
      *> [SkillModifier #willpower (-1), SanityModifier (-1)]
  getModifiersFor _ _ = pure []

instance HasAbilities DreamsOfRlyeh where
  getAbilities (DreamsOfRlyeh a) =
    [restrictedAbility a 1 OnSameLocation $ ActionAbility Nothing (ActionCost 1)]

instance RunMessage DreamsOfRlyeh where
  runMessage msg t@(DreamsOfRlyeh attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid attrs iid #willpower 3
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> DreamsOfRlyeh <$> runMessage msg attrs
