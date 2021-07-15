module Arkham.Types.Location.Cards.FloodedSquare
  ( floodedSquare
  , FloodedSquare(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol

newtype FloodedSquare = FloodedSquare LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

floodedSquare :: LocationCard FloodedSquare
floodedSquare = locationWith
  FloodedSquare
  Cards.floodedSquare
  0
  (Static 0)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasModifiersFor env FloodedSquare

instance ActionRunner env => HasActions env FloodedSquare where
  getActions iid window (FloodedSquare attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env FloodedSquare where
  runMessage msg (FloodedSquare attrs) = FloodedSquare <$> runMessage msg attrs
