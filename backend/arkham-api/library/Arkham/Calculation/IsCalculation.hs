module Arkham.Calculation.IsCalculation where

import Arkham.Prelude
import Arkham.Calculation

class IsCalculation a where
  toCalculation :: a -> GameCalculation

instance IsCalculation GameCalculation where
  toCalculation = id

instance IsCalculation Int where
  toCalculation = Fixed
