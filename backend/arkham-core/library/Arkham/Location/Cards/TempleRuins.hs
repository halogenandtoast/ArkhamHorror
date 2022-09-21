module Arkham.Location.Cards.TempleRuins
  ( templeRuins
  , TempleRuins(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TempleRuins = TempleRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeRuins :: LocationCard TempleRuins
templeRuins = location TempleRuins Cards.templeRuins 4 (Static 0)

instance HasAbilities TempleRuins where
  getAbilities (TempleRuins attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage TempleRuins where
  runMessage msg (TempleRuins attrs) = TempleRuins <$> runMessage msg attrs
