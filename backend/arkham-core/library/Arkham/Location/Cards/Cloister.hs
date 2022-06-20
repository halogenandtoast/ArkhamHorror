module Arkham.Location.Cards.Cloister
  ( cloister
  , Cloister(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype Cloister = Cloister LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloister :: LocationCard Cloister
cloister = location Cloister Cards.cloister 2 (PerPlayer 1) Heart [Square, Hourglass]

instance HasAbilities Cloister where
  getAbilities (Cloister attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage Cloister where
  runMessage msg (Cloister attrs) =
    Cloister <$> runMessage msg attrs
