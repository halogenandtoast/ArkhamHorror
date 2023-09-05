module Arkham.Location.Cards.FrenchHill_291
  ( frenchHill_291
  , FrenchHill_291(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype FrenchHill_291 = FrenchHill_291 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenchHill_291 :: LocationCard FrenchHill_291
frenchHill_291 = location FrenchHill_291 Cards.frenchHill_291 4 (Static 0)

instance HasAbilities FrenchHill_291 where
  getAbilities (FrenchHill_291 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage FrenchHill_291 where
  runMessage msg (FrenchHill_291 attrs) =
    FrenchHill_291 <$> runMessage msg attrs
