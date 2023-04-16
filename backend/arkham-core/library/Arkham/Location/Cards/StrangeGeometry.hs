module Arkham.Location.Cards.StrangeGeometry
  ( strangeGeometry
  , StrangeGeometry(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype StrangeGeometry = StrangeGeometry LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeGeometry :: LocationCard StrangeGeometry
strangeGeometry = location StrangeGeometry Cards.strangeGeometry 4 (Static 1)

instance HasAbilities StrangeGeometry where
  getAbilities (StrangeGeometry attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage StrangeGeometry where
  runMessage msg (StrangeGeometry attrs) =
    StrangeGeometry <$> runMessage msg attrs
