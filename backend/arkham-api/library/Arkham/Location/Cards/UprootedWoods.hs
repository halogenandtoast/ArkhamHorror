module Arkham.Location.Cards.UprootedWoods (uprootedWoods) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (uprootedWoods)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype UprootedWoods = UprootedWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uprootedWoods :: LocationCard UprootedWoods
uprootedWoods = location UprootedWoods Cards.uprootedWoods 2 (PerPlayer 1)

instance HasAbilities UprootedWoods where
  getAbilities (UprootedWoods a) =
    extendRevealed1 a
      $ restricted a 1 (youExist InvestigatorWithoutActionsRemaining)
      $ forced
      $ RevealLocation #after You (be a)

instance RunMessage UprootedWoods where
  runMessage msg l@(UprootedWoods attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardTopOfDeck iid (attrs.ability 1) 5
      pure l
    _ -> UprootedWoods <$> liftRunMessage msg attrs
