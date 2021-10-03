module Arkham.Types.Location.Cards.Kitchen
  ( kitchen
  , Kitchen(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype Kitchen = Kitchen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kitchen :: LocationCard Kitchen
kitchen = location Kitchen Cards.kitchen 2 (PerPlayer 1) Square [Triangle]

instance HasAbilities Kitchen where
  getAbilities (Kitchen attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env Kitchen where
  runMessage msg (Kitchen attrs) = Kitchen <$> runMessage msg attrs
