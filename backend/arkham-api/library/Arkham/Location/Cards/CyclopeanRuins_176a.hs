module Arkham.Location.Cards.CyclopeanRuins_176a (
  cyclopeanRuins_176a,
  CyclopeanRuins_176a (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype CyclopeanRuins_176a = CyclopeanRuins_176a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cyclopeanRuins_176a :: LocationCard CyclopeanRuins_176a
cyclopeanRuins_176a = locationWith CyclopeanRuins_176a Cards.cyclopeanRuins_176a 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities CyclopeanRuins_176a where
  getAbilities (CyclopeanRuins_176a attrs) =
    extendRevealed attrs []

instance RunMessage CyclopeanRuins_176a where
  runMessage msg (CyclopeanRuins_176a attrs) = runQueueT $ case msg of
    _ -> CyclopeanRuins_176a <$> liftRunMessage msg attrs
