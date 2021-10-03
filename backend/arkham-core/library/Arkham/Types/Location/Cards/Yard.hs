module Arkham.Types.Location.Cards.Yard
  ( yard
  , Yard(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype Yard = Yard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yard :: LocationCard Yard
yard = location Yard Cards.yard 1 (PerPlayer 1) Diamond [Circle, Plus]

instance HasAbilities Yard where
  getAbilities (Yard attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env Yard where
  runMessage msg (Yard attrs) = Yard <$> runMessage msg attrs
