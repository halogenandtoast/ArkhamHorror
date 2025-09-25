module Arkham.Location.Cards.BilliardsRoom (billiardsRoom) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BilliardsRoom = BilliardsRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

billiardsRoom :: LocationCard BilliardsRoom
billiardsRoom = location BilliardsRoom Cards.billiardsRoom 3 (Static 0)

instance HasAbilities BilliardsRoom where
  getAbilities (BilliardsRoom a) =
    extendRevealed1 a $ playerLimit PerRound $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage BilliardsRoom where
  runMessage msg l@(BilliardsRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      gainClues iid (attrs.ability 1) 1
      pure l
    _ -> BilliardsRoom <$> liftRunMessage msg attrs
