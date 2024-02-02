module Arkham.Location.Cards.BackAlley (
  backAlley,
  BackAlley (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (backAlley)
import Arkham.Location.Runner

newtype BackAlley = BackAlley LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

backAlley :: LocationCard BackAlley
backAlley = location BackAlley Cards.backAlley 1 (PerPlayer 1)

instance HasAbilities BackAlley where
  getAbilities (BackAlley a) = withBaseAbilities a [locationResignAction a]

instance RunMessage BackAlley where
  runMessage msg (BackAlley attrs) = BackAlley <$> runMessage msg attrs
