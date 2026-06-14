module Arkham.Location.Cards.LostCampsite (lostCampsite) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LostCampsite = LostCampsite LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostCampsite :: LocationCard LostCampsite
lostCampsite = location LostCampsite Cards.lostCampsite 4 (Static 1)

instance HasAbilities LostCampsite where
  getAbilities (LostCampsite a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (be a <> LocationWithCardsUnderneath (LengthIs $ atLeast 1)))
      $ actionAbilityWithCost (ActionCost 1)

instance RunMessage LostCampsite where
  runMessage msg l@(LostCampsite attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ (listToMaybe attrs.underneath) \card -> addToHand iid (only card)
      pure l
    _ -> LostCampsite <$> liftRunMessage msg attrs
