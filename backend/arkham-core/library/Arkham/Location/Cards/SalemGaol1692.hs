module Arkham.Location.Cards.SalemGaol1692
  ( salemGaol1692
  , SalemGaol1692(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SalemGaol1692 = SalemGaol1692 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

salemGaol1692 :: LocationCard SalemGaol1692
salemGaol1692 = location SalemGaol1692 Cards.salemGaol1692 3 (PerPlayer 1)

instance HasAbilities SalemGaol1692 where
  getAbilities (SalemGaol1692 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage SalemGaol1692 where
  runMessage msg (SalemGaol1692 attrs) = SalemGaol1692 <$> runMessage msg attrs
