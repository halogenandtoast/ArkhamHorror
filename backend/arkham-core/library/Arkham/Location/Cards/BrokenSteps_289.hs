module Arkham.Location.Cards.BrokenSteps_289
  ( brokenSteps_289
  , BrokenSteps_289(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype BrokenSteps_289 = BrokenSteps_289 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenSteps_289 :: LocationCard BrokenSteps_289
brokenSteps_289 = location
  BrokenSteps_289
  Cards.brokenSteps_289
  4
  (Static 0)
  Equals
  [Squiggle, Triangle, Diamond, Square]

instance HasAbilities BrokenSteps_289 where
  getAbilities (BrokenSteps_289 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BrokenSteps_289 where
  runMessage msg (BrokenSteps_289 attrs) =
    BrokenSteps_289 <$> runMessage msg attrs
