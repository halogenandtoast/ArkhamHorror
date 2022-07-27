module Arkham.Location.Cards.OvergrownRuins
  ( overgrownRuins
  , OvergrownRuins(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype OvergrownRuins = OvergrownRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overgrownRuins :: LocationCard OvergrownRuins
overgrownRuins = location
  OvergrownRuins
  Cards.overgrownRuins
  5
  (PerPlayer 1)
  T
  [Moon, Heart, Equals]

instance HasAbilities OvergrownRuins where
  getAbilities (OvergrownRuins attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage OvergrownRuins where
  runMessage msg (OvergrownRuins attrs) =
    OvergrownRuins <$> runMessage msg attrs
