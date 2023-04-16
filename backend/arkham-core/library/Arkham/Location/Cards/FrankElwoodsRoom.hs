module Arkham.Location.Cards.FrankElwoodsRoom
  ( frankElwoodsRoom
  , FrankElwoodsRoom(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype FrankElwoodsRoom = FrankElwoodsRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frankElwoodsRoom :: LocationCard FrankElwoodsRoom
frankElwoodsRoom =
  location FrankElwoodsRoom Cards.frankElwoodsRoom 3 (PerPlayer 1)

instance HasAbilities FrankElwoodsRoom where
  getAbilities (FrankElwoodsRoom attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage FrankElwoodsRoom where
  runMessage msg (FrankElwoodsRoom attrs) =
    FrankElwoodsRoom <$> runMessage msg attrs
