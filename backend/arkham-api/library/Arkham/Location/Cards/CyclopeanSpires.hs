module Arkham.Location.Cards.CyclopeanSpires (cyclopeanSpires) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CyclopeanSpires = CyclopeanSpires LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cyclopeanSpires :: LocationCard CyclopeanSpires
cyclopeanSpires = location CyclopeanSpires Cards.cyclopeanSpires 3 (PerPlayer 1)

instance HasAbilities CyclopeanSpires where
  getAbilities (CyclopeanSpires attrs) =
    extendRevealed attrs []

instance RunMessage CyclopeanSpires where
  runMessage msg (CyclopeanSpires attrs) = runQueueT $ case msg of
    _ -> CyclopeanSpires <$> liftRunMessage msg attrs
