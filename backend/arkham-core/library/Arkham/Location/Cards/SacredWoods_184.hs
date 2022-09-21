module Arkham.Location.Cards.SacredWoods_184
  ( sacredWoods_184
  , SacredWoods_184(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SacredWoods_184 = SacredWoods_184 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacredWoods_184 :: LocationCard SacredWoods_184
sacredWoods_184 =
  location SacredWoods_184 Cards.sacredWoods_184 4 (PerPlayer 1)

instance HasAbilities SacredWoods_184 where
  getAbilities (SacredWoods_184 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage SacredWoods_184 where
  runMessage msg (SacredWoods_184 attrs) =
    SacredWoods_184 <$> runMessage msg attrs
