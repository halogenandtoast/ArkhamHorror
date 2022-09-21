module Arkham.Location.Cards.SacredWoods_185
  ( sacredWoods_185
  , SacredWoods_185(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SacredWoods_185 = SacredWoods_185 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacredWoods_185 :: LocationCard SacredWoods_185
sacredWoods_185 =
  location SacredWoods_185 Cards.sacredWoods_185 6 (PerPlayer 1)

instance HasAbilities SacredWoods_185 where
  getAbilities (SacredWoods_185 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage SacredWoods_185 where
  runMessage msg (SacredWoods_185 attrs) =
    SacredWoods_185 <$> runMessage msg attrs
