module Arkham.Location.Cards.BilliardsRoom (billiardsRoom, BilliardsRoom (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Prelude

newtype BilliardsRoom = BilliardsRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

billiardsRoom :: LocationCard BilliardsRoom
billiardsRoom = location BilliardsRoom Cards.billiardsRoom 3 (Static 0)

instance HasAbilities BilliardsRoom where
  getAbilities (BilliardsRoom a) =
    withBaseAbilities
      a
      [playerLimit PerRound $ skillTestAbility $ restrictedAbility a 1 Here actionAbility]

instance RunMessage BilliardsRoom where
  runMessage msg l@(BilliardsRoom attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ GainClues iid (toAbilitySource attrs 1) 1
      pure l
    _ -> BilliardsRoom <$> runMessage msg attrs
