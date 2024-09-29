module Arkham.Location.Cards.TempleOfTheUnion_177b
  ( templeOfTheUnion_177b
  , TempleOfTheUnion_177b(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TempleOfTheUnion_177b = TempleOfTheUnion_177b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeOfTheUnion_177b :: LocationCard TempleOfTheUnion_177b
templeOfTheUnion_177b = location TempleOfTheUnion_177b Cards.templeOfTheUnion_177b 3 (PerPlayer 2)

instance HasAbilities TempleOfTheUnion_177b where
  getAbilities (TempleOfTheUnion_177b attrs) =
    extendRevealed attrs []

instance RunMessage TempleOfTheUnion_177b where
  runMessage msg (TempleOfTheUnion_177b attrs) = runQueueT $ case msg of
    _ -> TempleOfTheUnion_177b <$> liftRunMessage msg attrs
