module Arkham.Location.Cards.APocketInTime
  ( aPocketInTime
  , APocketInTime(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype APocketInTime = APocketInTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aPocketInTime :: LocationCard APocketInTime
aPocketInTime = location APocketInTime Cards.aPocketInTime 5 (PerPlayer 1)

instance HasAbilities APocketInTime where
  getAbilities (APocketInTime attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage APocketInTime where
  runMessage msg (APocketInTime attrs) = APocketInTime <$> runMessage msg attrs
