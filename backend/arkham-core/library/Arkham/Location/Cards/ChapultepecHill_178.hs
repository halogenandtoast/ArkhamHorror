module Arkham.Location.Cards.ChapultepecHill_178
  ( chapultepecHill_178
  , ChapultepecHill_178(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ChapultepecHill_178 = ChapultepecHill_178 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapultepecHill_178 :: LocationCard ChapultepecHill_178
chapultepecHill_178 = location ChapultepecHill_178 Cards.chapultepecHill_178 2 (PerPlayer 2)

instance HasAbilities ChapultepecHill_178 where
  getAbilities (ChapultepecHill_178 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage ChapultepecHill_178 where
  runMessage msg (ChapultepecHill_178 attrs) =
    ChapultepecHill_178 <$> runMessage msg attrs
