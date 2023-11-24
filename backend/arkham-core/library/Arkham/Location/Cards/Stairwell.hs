module Arkham.Location.Cards.Stairwell
  ( stairwell
  , Stairwell(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Stairwell = Stairwell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stairwell :: LocationCard Stairwell
stairwell = location Stairwell Cards.stairwell 3 (PerPlayer 1)

instance HasAbilities Stairwell where
  getAbilities (Stairwell attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Stairwell where
  runMessage msg (Stairwell attrs) =
    Stairwell <$> runMessage msg attrs
