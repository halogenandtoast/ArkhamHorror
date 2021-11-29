module Arkham.Types.Location.Cards.CanalSaintMartin
  ( canalSaintMartin
  , CanalSaintMartin(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype CanalSaintMartin = CanalSaintMartin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

canalSaintMartin :: LocationCard CanalSaintMartin
canalSaintMartin = location
  CanalSaintMartin
  Cards.canalSaintMartin
  4
  (PerPlayer 1)
  Equals
  [Square, T, Moon]

instance HasAbilities CanalSaintMartin where
  getAbilities (CanalSaintMartin attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env CanalSaintMartin where
  runMessage msg (CanalSaintMartin attrs) =
    CanalSaintMartin <$> runMessage msg attrs
