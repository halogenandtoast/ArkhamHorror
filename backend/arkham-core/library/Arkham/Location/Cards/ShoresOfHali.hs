module Arkham.Location.Cards.ShoresOfHali
  ( shoresOfHali
  , ShoresOfHali(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import qualified Arkham.Location.Cards as Cards
import Arkham.Location.Runner

newtype ShoresOfHali = ShoresOfHali LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

shoresOfHali :: LocationCard ShoresOfHali
shoresOfHali =
  location ShoresOfHali Cards.shoresOfHali 3 (PerPlayer 2) Circle [Square]

instance RunMessage ShoresOfHali where
  runMessage msg (ShoresOfHali attrs) = ShoresOfHali <$> runMessage msg attrs
