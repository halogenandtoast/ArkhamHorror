module Arkham.Types.Location.Cards.CanalSide
  ( canalSide
  , CanalSide(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol

newtype CanalSide = CanalSide LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

canalSide :: LocationCard CanalSide
canalSide = locationWith
  CanalSide
  Cards.canalSide
  0
  (Static 0)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasModifiersFor env CanalSide

instance ActionRunner env => HasActions env CanalSide where
  getActions iid window (CanalSide attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env CanalSide where
  runMessage msg (CanalSide attrs) = CanalSide <$> runMessage msg attrs
