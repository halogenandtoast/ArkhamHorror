module Arkham.Types.Location.Cards.BackAlley
  ( backAlley
  , BackAlley(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (backAlley)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype BackAlley = BackAlley LocationAttrs
  deriving anyclass IsLocation
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

instance HasModifiersFor env BackAlley

instance HasAbilities env BackAlley where
  getAbilities _ _ (BackAlley a) = pure [locationResignAction a]

instance LocationRunner env => RunMessage env BackAlley where
  runMessage msg (BackAlley attrs) = BackAlley <$> runMessage msg attrs
