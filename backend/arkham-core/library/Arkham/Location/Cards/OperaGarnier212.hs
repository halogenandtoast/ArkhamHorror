module Arkham.Location.Cards.OperaGarnier212
  ( operaGarnier212
  , OperaGarnier212(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype OperaGarnier212 = OperaGarnier212 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

operaGarnier212 :: LocationCard OperaGarnier212
operaGarnier212 = location
  OperaGarnier212
  Cards.operaGarnier212
  5
  (PerPlayer 1)
  Diamond
  [Triangle, Square, Heart]

instance HasAbilities OperaGarnier212 where
  getAbilities (OperaGarnier212 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env OperaGarnier212 where
  runMessage msg (OperaGarnier212 attrs) =
    OperaGarnier212 <$> runMessage msg attrs
