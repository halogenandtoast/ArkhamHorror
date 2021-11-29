module Arkham.Types.Location.Cards.OperaGarnier213
  ( operaGarnier213
  , OperaGarnier213(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype OperaGarnier213 = OperaGarnier213 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

operaGarnier213 :: LocationCard OperaGarnier213
operaGarnier213 = location
  OperaGarnier213
  Cards.operaGarnier213
  6
  (PerPlayer 1)
  Diamond
  [Triangle, Square, Heart]

instance HasAbilities OperaGarnier213 where
  getAbilities (OperaGarnier213 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env OperaGarnier213 where
  runMessage msg (OperaGarnier213 attrs) =
    OperaGarnier213 <$> runMessage msg attrs
