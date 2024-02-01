module Arkham.Location.Cards.MiskatonicQuad (
  MiskatonicQuad (..),
  miskatonicQuad,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (miskatonicQuad)
import Arkham.Location.Runner

newtype MiskatonicQuad = MiskatonicQuad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

miskatonicQuad :: LocationCard MiskatonicQuad
miskatonicQuad = location MiskatonicQuad Cards.miskatonicQuad 3 (Static 0)

instance HasAbilities MiskatonicQuad where
  getAbilities (MiskatonicQuad a) =
    withBaseAbilities a $ [locationResignAction a]

instance RunMessage MiskatonicQuad where
  runMessage msg (MiskatonicQuad attrs) =
    MiskatonicQuad <$> runMessage msg attrs
