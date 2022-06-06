module Arkham.Location.Cards.Foyer
  ( foyer
  , Foyer(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype Foyer = Foyer LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foyer :: LocationCard Foyer
foyer = location Foyer Cards.foyer 2 (PerPlayer 1) T [Circle, Square, Equals]

instance HasAbilities Foyer where
  getAbilities (Foyer attrs) = withResignAction attrs []

instance RunMessage Foyer where
  runMessage msg (Foyer attrs) = Foyer <$> runMessage msg attrs
