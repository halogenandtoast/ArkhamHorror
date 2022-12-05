module Arkham.Location.Cards.Yuggoth
  ( yuggoth
  , Yuggoth(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Yuggoth = Yuggoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yuggoth :: LocationCard Yuggoth
yuggoth = location Yuggoth Cards.yuggoth 2 (Static 3)

instance HasAbilities Yuggoth where
  getAbilities (Yuggoth attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage Yuggoth where
  runMessage msg (Yuggoth attrs) = Yuggoth <$> runMessage msg attrs
