module Arkham.Types.Location.Cards.DiningRoom
  ( diningRoom
  , DiningRoom(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype DiningRoom = DiningRoom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diningRoom :: LocationCard DiningRoom
diningRoom = location DiningRoom Cards.diningRoom 0 (Static 0) NoSymbol []

instance HasModifiersFor env DiningRoom

instance HasAbilities DiningRoom where
  getAbilities (DiningRoom attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env DiningRoom where
  runMessage msg (DiningRoom attrs) = DiningRoom <$> runMessage msg attrs
