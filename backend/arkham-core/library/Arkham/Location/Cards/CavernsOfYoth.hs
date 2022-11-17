module Arkham.Location.Cards.CavernsOfYoth
  ( cavernsOfYoth
  , CavernsOfYoth(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CavernsOfYoth = CavernsOfYoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cavernsOfYoth :: LocationCard CavernsOfYoth
cavernsOfYoth =
  symbolLabel $ location CavernsOfYoth Cards.cavernsOfYoth 1 (PerPlayer 1)

instance HasAbilities CavernsOfYoth where
  getAbilities (CavernsOfYoth attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage CavernsOfYoth where
  runMessage msg (CavernsOfYoth attrs) = CavernsOfYoth <$> runMessage msg attrs
