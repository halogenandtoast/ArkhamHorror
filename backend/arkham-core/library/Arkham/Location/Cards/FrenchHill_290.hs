module Arkham.Location.Cards.FrenchHill_290
  ( frenchHill_290
  , FrenchHill_290(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype FrenchHill_290 = FrenchHill_290 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenchHill_290 :: LocationCard FrenchHill_290
frenchHill_290 = location FrenchHill_290 Cards.frenchHill_290 3 (Static 0)

instance HasAbilities FrenchHill_290 where
  getAbilities (FrenchHill_290 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage FrenchHill_290 where
  runMessage msg (FrenchHill_290 attrs) =
    FrenchHill_290 <$> runMessage msg attrs
