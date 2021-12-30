module Arkham.Location.Cards.MiskatonicQuad
  ( MiskatonicQuad(..)
  , miskatonicQuad
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (miskatonicQuad)
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Attrs

newtype MiskatonicQuad = MiskatonicQuad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicQuad :: LocationCard MiskatonicQuad
miskatonicQuad = location
  MiskatonicQuad
  Cards.miskatonicQuad
  3
  (Static 0)
  Plus
  [Triangle, Hourglass, Square, Diamond, Circle]

instance HasAbilities MiskatonicQuad where
  getAbilities (MiskatonicQuad a) =
    withBaseAbilities a $ [locationResignAction a]

instance (LocationRunner env) => RunMessage env MiskatonicQuad where
  runMessage msg (MiskatonicQuad attrs) =
    MiskatonicQuad <$> runMessage msg attrs
