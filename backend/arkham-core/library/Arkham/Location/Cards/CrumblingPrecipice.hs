module Arkham.Location.Cards.CrumblingPrecipice
  ( crumblingPrecipice
  , CrumblingPrecipice(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CrumblingPrecipice = CrumblingPrecipice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crumblingPrecipice :: LocationCard CrumblingPrecipice
crumblingPrecipice = symbolLabel
  $ location CrumblingPrecipice Cards.crumblingPrecipice 4 (Static 0)

instance HasAbilities CrumblingPrecipice where
  getAbilities (CrumblingPrecipice attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage CrumblingPrecipice where
  runMessage msg (CrumblingPrecipice attrs) =
    CrumblingPrecipice <$> runMessage msg attrs
