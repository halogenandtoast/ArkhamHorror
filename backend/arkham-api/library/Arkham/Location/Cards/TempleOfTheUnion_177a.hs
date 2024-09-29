module Arkham.Location.Cards.TempleOfTheUnion_177a (
  templeOfTheUnion_177a,
  TempleOfTheUnion_177a (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype TempleOfTheUnion_177a = TempleOfTheUnion_177a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeOfTheUnion_177a :: LocationCard TempleOfTheUnion_177a
templeOfTheUnion_177a = locationWith TempleOfTheUnion_177a Cards.templeOfTheUnion_177a 5 (PerPlayer 1) connectsToAdjacent

instance HasAbilities TempleOfTheUnion_177a where
  getAbilities (TempleOfTheUnion_177a attrs) =
    extendRevealed attrs []

instance RunMessage TempleOfTheUnion_177a where
  runMessage msg (TempleOfTheUnion_177a attrs) = runQueueT $ case msg of
    _ -> TempleOfTheUnion_177a <$> liftRunMessage msg attrs
