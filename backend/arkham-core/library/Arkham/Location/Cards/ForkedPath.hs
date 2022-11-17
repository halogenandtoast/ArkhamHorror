module Arkham.Location.Cards.ForkedPath
  ( forkedPath
  , ForkedPath(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ForkedPath = ForkedPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forkedPath :: LocationCard ForkedPath
forkedPath = symbolLabel $ location ForkedPath Cards.forkedPath 2 (PerPlayer 2)

instance HasAbilities ForkedPath where
  getAbilities (ForkedPath attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage ForkedPath where
  runMessage msg (ForkedPath attrs) = ForkedPath <$> runMessage msg attrs
