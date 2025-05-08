module Arkham.Location.Cards.SlaughteredWoods (slaughteredWoods) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (slaughteredWoods)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SlaughteredWoods = SlaughteredWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slaughteredWoods :: LocationCard SlaughteredWoods
slaughteredWoods = location SlaughteredWoods Cards.slaughteredWoods 2 (PerPlayer 1)

instance HasAbilities SlaughteredWoods where
  getAbilities (SlaughteredWoods a) =
    extendRevealed1 a
      $ restricted a 1 (youExist InvestigatorWithoutActionsRemaining)
      $ forced $ RevealLocation #after You (be a)

instance RunMessage SlaughteredWoods where
  runMessage msg l@(SlaughteredWoods attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 2) 2
      pure l
    _ -> SlaughteredWoods <$> liftRunMessage msg attrs
