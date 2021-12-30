module Arkham.Location.Cards.BackAlley
  ( backAlley
  , BackAlley(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (backAlley)
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Runner

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
