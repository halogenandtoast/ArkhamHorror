module Arkham.Types.Location.Cards.StreetsOfVenice
  ( streetsOfVenice
  , StreetsOfVenice(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol

newtype StreetsOfVenice = StreetsOfVenice LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetsOfVenice :: LocationCard StreetsOfVenice
streetsOfVenice = locationWith
  StreetsOfVenice
  Cards.streetsOfVenice
  2
  (Static 2)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasModifiersFor env StreetsOfVenice

instance ActionRunner env => HasActions env StreetsOfVenice where
  getActions iid window (StreetsOfVenice attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env StreetsOfVenice where
  runMessage msg (StreetsOfVenice attrs) =
    StreetsOfVenice <$> runMessage msg attrs
