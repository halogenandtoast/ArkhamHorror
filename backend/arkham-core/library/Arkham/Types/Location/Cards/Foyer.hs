module Arkham.Types.Location.Cards.Foyer
  ( foyer
  , Foyer(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype Foyer = Foyer LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foyer :: LocationCard Foyer
foyer = location Foyer Cards.foyer 2 (PerPlayer 1) T [Circle, Square, Equals]

instance HasAbilities Foyer where
  getAbilities (Foyer attrs) = withResignAction attrs []

instance LocationRunner env => RunMessage env Foyer where
  runMessage msg (Foyer attrs) = Foyer <$> runMessage msg attrs
