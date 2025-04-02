module Arkham.Location.Cards.UndercityAltar (undercityAltar) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype UndercityAltar = UndercityAltar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undercityAltar :: LocationCard UndercityAltar
undercityAltar = location UndercityAltar Cards.undercityAltar 3 (Static 0)

instance HasAbilities UndercityAltar where
  getAbilities (UndercityAltar attrs) =
    extendRevealed attrs []

instance RunMessage UndercityAltar where
  runMessage msg (UndercityAltar attrs) = runQueueT $ case msg of
    _ -> UndercityAltar <$> liftRunMessage msg attrs
