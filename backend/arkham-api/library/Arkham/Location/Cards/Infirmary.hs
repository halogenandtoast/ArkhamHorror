module Arkham.Location.Cards.Infirmary (infirmary) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose

newtype Infirmary = Infirmary LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infirmary :: LocationCard Infirmary
infirmary = location Infirmary Cards.infirmary 3 (PerPlayer 1)

instance HasAbilities Infirmary where
  getAbilities (Infirmary attrs) =
    extendRevealed1 attrs $ playerLimit PerRound $ restricted attrs 1 Here actionAbility

instance RunMessage Infirmary where
  runMessage msg l@(Infirmary attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      canHealDamage <- canHaveDamageHealed source iid
      canHealHorror <- canHaveHorrorHealed source iid
      chooseOneM iid do
        labeled "Heal 1 damage and take 1 direct horror" do
          when canHealDamage $ healDamage iid source 1
          directHorror iid source 1
        labeled "Heal 1 horror and take 1 direct damage" do
          when canHealHorror $ healHorror iid source 1
          directDamage iid source 1
      pure l
    _ -> Infirmary <$> liftRunMessage msg attrs
