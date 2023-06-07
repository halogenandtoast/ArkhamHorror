module Arkham.Location.Cards.Library
  ( library
  , Library(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Library = Library LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

library :: LocationCard Library
library = location Library Cards.library 6 (PerPlayer 1)

instance HasAbilities Library where
  getAbilities (Library attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Library where
  runMessage msg (Library attrs) =
    Library <$> runMessage msg attrs
