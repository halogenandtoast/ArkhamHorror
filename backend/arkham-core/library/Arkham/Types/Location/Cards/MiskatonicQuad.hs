module Arkham.Types.Location.Cards.MiskatonicQuad
  ( MiskatonicQuad(..)
  , miskatonicQuad
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (miskatonicQuad)
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

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
