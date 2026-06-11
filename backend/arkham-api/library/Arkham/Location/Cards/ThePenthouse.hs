module Arkham.Location.Cards.ThePenthouse (thePenthouse) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ThePenthouse = ThePenthouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePenthouse :: LocationCard ThePenthouse
thePenthouse = symbolLabel $ location ThePenthouse Cards.thePenthouse 2 (PerPlayer 1)

instance HasAbilities ThePenthouse where
  getAbilities (ThePenthouse a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage ThePenthouse where
  runMessage msg l@(ThePenthouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      gainClues iid (attrs.ability 1) 1
      pure l
    _ -> ThePenthouse <$> liftRunMessage msg attrs
