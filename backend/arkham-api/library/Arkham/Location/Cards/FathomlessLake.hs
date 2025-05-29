module Arkham.Location.Cards.FathomlessLake (fathomlessLake) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FathomlessLake = FathomlessLake LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fathomlessLake :: LocationCard FathomlessLake
fathomlessLake = location FathomlessLake Cards.fathomlessLake 4 (PerPlayer 1)

instance HasAbilities FathomlessLake where
  getAbilities (FathomlessLake a) =
    extendRevealed1 a
      $ restricted a 1 (youExist $ InvestigatorWithActionsRemaining (atLeast 2))
      $ forced
      $ RevealLocation #after You (be a)

instance RunMessage FathomlessLake where
  runMessage msg l@(FathomlessLake attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeDoomOnAgenda 1
      pure l
    _ -> FathomlessLake <$> liftRunMessage msg attrs
