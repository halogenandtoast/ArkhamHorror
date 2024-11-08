module Arkham.Location.Cards.FoulCorridors (foulCorridors, FoulCorridors (..)) where

import Arkham.Ability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.ScenarioLogKey

newtype FoulCorridors = FoulCorridors LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foulCorridors :: LocationCard FoulCorridors
foulCorridors = location FoulCorridors Cards.foulCorridors 2 (Static 0)

instance HasAbilities FoulCorridors where
  getAbilities (FoulCorridors a) =
    extendRevealed1 a $ restricted a 1 Here $ actionAbilityWithCost (SpendKeyCost BlackKey)

instance RunMessage FoulCorridors where
  runMessage msg l@(FoulCorridors attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      remember UnlockedTheThirdFloor
      pure l
    _ -> FoulCorridors <$> liftRunMessage msg attrs
