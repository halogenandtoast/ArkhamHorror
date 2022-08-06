module Arkham.Location.Cards.Entryway
  ( entryway
  , Entryway(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Entryway = Entryway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entryway :: LocationCard Entryway
entryway = location Entryway Cards.entryway 2 (PerPlayer 1)

instance HasAbilities Entryway where
  getAbilities (Entryway attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage Entryway where
  runMessage msg (Entryway attrs) = Entryway <$> runMessage msg attrs
