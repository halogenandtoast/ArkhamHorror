module Arkham.Location.Cards.ChapultepecHill_179
  ( chapultepecHill_179
  , ChapultepecHill_179(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ChapultepecHill_179 = ChapultepecHill_179 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapultepecHill_179 :: LocationCard ChapultepecHill_179
chapultepecHill_179 =
  location ChapultepecHill_179 Cards.chapultepecHill_179 4 (PerPlayer 1)

instance HasAbilities ChapultepecHill_179 where
  getAbilities (ChapultepecHill_179 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage ChapultepecHill_179 where
  runMessage msg (ChapultepecHill_179 attrs) =
    ChapultepecHill_179 <$> runMessage msg attrs
