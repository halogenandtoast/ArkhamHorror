module Arkham.Location.Cards.TowerOfKoth (towerOfKoth, TowerOfKoth (..)) where

import Arkham.Ability
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Story.Cards qualified as Story

newtype TowerOfKoth = TowerOfKoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

towerOfKoth :: LocationCard TowerOfKoth
towerOfKoth = location TowerOfKoth Cards.towerOfKoth 5 (Static 0)

instance HasAbilities TowerOfKoth where
  getAbilities (TowerOfKoth attrs) = extendRevealed attrs [restrictedAbility attrs 1 Here actionAbility]

instance RunMessage TowerOfKoth where
  runMessage msg l@(TowerOfKoth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      beginSkillTest iid (attrs.ability 1) iid #combat (Fixed 5)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      when (locationCanBeFlipped attrs)
        $ flipOver iid attrs
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Story.anotherPath
      pure . TowerOfKoth $ attrs & canBeFlippedL .~ False
    _ -> TowerOfKoth <$> liftRunMessage msg attrs
