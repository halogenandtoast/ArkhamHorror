module Arkham.Types.Location.Cards.TearThroughTime
  ( tearThroughTime
  , TearThroughTime(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Window

newtype TearThroughTime = TearThroughTime LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tearThroughTime :: LocationCard TearThroughTime
tearThroughTime = location
  TearThroughTime
  Cards.tearThroughTime
  2
  (PerPlayer 2)
  Moon
  [Circle, Plus, Squiggle]

instance HasModifiersFor env TearThroughTime

instance ActionRunner env => HasActions env TearThroughTime where
  getActions iid NonFast (TearThroughTime attrs) =
    withBaseActions iid NonFast attrs $ pure [resignAction iid attrs]
  getActions iid window (TearThroughTime attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env TearThroughTime where
  runMessage msg (TearThroughTime attrs) =
    TearThroughTime <$> runMessage msg attrs
