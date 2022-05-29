module Arkham.Location.Cards.NarrowShaft
  ( narrowShaft
  , NarrowShaft(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype NarrowShaft = NarrowShaft LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narrowShaft :: LocationCard NarrowShaft
narrowShaft = location NarrowShaft Cards.narrowShaft 0 (Static 0) NoSymbol []

instance HasAbilities NarrowShaft where
  getAbilities (NarrowShaft attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance LocationRunner env => RunMessage env NarrowShaft where
  runMessage msg (NarrowShaft attrs) =
    NarrowShaft <$> runMessage msg attrs
