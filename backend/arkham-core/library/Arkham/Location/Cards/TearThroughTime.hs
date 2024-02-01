module Arkham.Location.Cards.TearThroughTime (
  tearThroughTime,
  TearThroughTime (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype TearThroughTime = TearThroughTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

tearThroughTime :: LocationCard TearThroughTime
tearThroughTime =
  location TearThroughTime Cards.tearThroughTime 2 (PerPlayer 2)

instance HasAbilities TearThroughTime where
  getAbilities (TearThroughTime attrs) =
    withBaseAbilities attrs $ [resignAction attrs]

instance RunMessage TearThroughTime where
  runMessage msg (TearThroughTime attrs) =
    TearThroughTime <$> runMessage msg attrs
