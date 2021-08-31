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

newtype TearThroughTime = TearThroughTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tearThroughTime :: LocationCard TearThroughTime
tearThroughTime = location
  TearThroughTime
  Cards.tearThroughTime
  2
  (PerPlayer 2)
  Moon
  [Circle, Plus, Squiggle]

instance HasAbilities env TearThroughTime where
  getAbilities iid window (TearThroughTime attrs) =
    withBaseAbilities iid window attrs $ pure [resignAction attrs]

instance LocationRunner env => RunMessage env TearThroughTime where
  runMessage msg (TearThroughTime attrs) =
    TearThroughTime <$> runMessage msg attrs
