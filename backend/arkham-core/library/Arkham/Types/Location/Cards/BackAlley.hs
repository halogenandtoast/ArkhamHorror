module Arkham.Types.Location.Cards.BackAlley
  ( backAlley
  , BackAlley(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (backAlley)
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype BackAlley = BackAlley LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backAlley :: LocationCard BackAlley
backAlley = locationWith
  BackAlley
  Cards.backAlley
  1
  (PerPlayer 1)
  T
  [Diamond]
  (revealedSymbolL .~ Squiggle)

instance HasAbilities BackAlley where
  getAbilities (BackAlley a) = withBaseAbilities a [locationResignAction a]

instance LocationRunner env => RunMessage env BackAlley where
  runMessage msg (BackAlley attrs) = BackAlley <$> runMessage msg attrs
