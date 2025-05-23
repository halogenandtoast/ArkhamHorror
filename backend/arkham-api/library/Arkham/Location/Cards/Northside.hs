module Arkham.Location.Cards.Northside (northside) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (northside)
import Arkham.Location.Import.Lifted

newtype Northside = Northside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northside :: LocationCard Northside
northside = location Northside Cards.northside 3 (PerPlayer 2)

instance HasAbilities Northside where
  getAbilities (Northside x) =
    extendRevealed1 x
      $ groupLimit PerGame
      $ restricted x 1 Here
      $ actionAbilityWithCost (ResourceCost 5)

instance RunMessage Northside where
  runMessage msg l@(Northside attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainClues iid (attrs.ability 1) 2
      pure l
    _ -> Northside <$> liftRunMessage msg attrs
