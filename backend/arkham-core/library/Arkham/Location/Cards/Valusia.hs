module Arkham.Location.Cards.Valusia
  ( valusia
  , Valusia(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Valusia = Valusia LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valusia :: LocationCard Valusia
valusia = location Valusia Cards.valusia 4 (Static 2)

instance HasAbilities Valusia where
  getAbilities (Valusia attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage Valusia where
  runMessage msg (Valusia attrs) = Valusia <$> runMessage msg attrs
