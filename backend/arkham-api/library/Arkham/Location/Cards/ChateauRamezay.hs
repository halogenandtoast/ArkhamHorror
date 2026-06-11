module Arkham.Location.Cards.ChateauRamezay (chateauRamezay) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ChateauRamezay = ChateauRamezay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chateauRamezay :: LocationCard ChateauRamezay
chateauRamezay = symbolLabel $ location ChateauRamezay Cards.chateauRamezay 2 (PerPlayer 1)

instance HasAbilities ChateauRamezay where
  getAbilities (ChateauRamezay a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage ChateauRamezay where
  runMessage msg l@(ChateauRamezay attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      gainClues iid (attrs.ability 1) 1
      pure l
    _ -> ChateauRamezay <$> liftRunMessage msg attrs
