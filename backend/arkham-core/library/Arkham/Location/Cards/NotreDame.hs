module Arkham.Location.Cards.NotreDame
  ( notreDame
  , NotreDame(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs

newtype NotreDame = NotreDame LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

notreDame :: LocationCard NotreDame
notreDame =
  location NotreDame Cards.notreDame 3 (PerPlayer 1) Plus [Circle, Moon, Star]

instance HasAbilities NotreDame where
  getAbilities (NotreDame attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env NotreDame where
  runMessage msg (NotreDame attrs) = NotreDame <$> runMessage msg attrs
