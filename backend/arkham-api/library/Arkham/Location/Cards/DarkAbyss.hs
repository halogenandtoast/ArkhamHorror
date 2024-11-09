module Arkham.Location.Cards.DarkAbyss (darkAbyss, DarkAbyss (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype DarkAbyss = DarkAbyss LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkAbyss :: LocationCard DarkAbyss
darkAbyss = locationWith DarkAbyss Cards.darkAbyss 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities DarkAbyss where
  getAbilities (DarkAbyss attrs) =
    extendRevealed attrs []

instance RunMessage DarkAbyss where
  runMessage msg (DarkAbyss attrs) = runQueueT $ case msg of
    _ -> DarkAbyss <$> liftRunMessage msg attrs
