module Arkham.Location.Cards.BrokenSteps_290
  ( brokenSteps_290
  , BrokenSteps_290(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype BrokenSteps_290 = BrokenSteps_290 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenSteps_290 :: LocationCard BrokenSteps_290
brokenSteps_290 = location
  BrokenSteps_290
  Cards.brokenSteps_290
  3
  (Static 0)
  Equals
  [Squiggle, Triangle, Diamond, Square]

instance HasAbilities BrokenSteps_290 where
  getAbilities (BrokenSteps_290 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BrokenSteps_290 where
  runMessage msg (BrokenSteps_290 attrs) =
    BrokenSteps_290 <$> runMessage msg attrs
