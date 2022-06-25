module Arkham.Location.Cards.DepthsOfDemheStepsOfThePalace
  ( depthsOfDemheStepsOfThePalace
  , DepthsOfDemheStepsOfThePalace(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype DepthsOfDemheStepsOfThePalace = DepthsOfDemheStepsOfThePalace LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

depthsOfDemheStepsOfThePalace :: LocationCard DepthsOfDemheStepsOfThePalace
depthsOfDemheStepsOfThePalace = location
  DepthsOfDemheStepsOfThePalace
  Cards.depthsOfDemheStepsOfThePalace
  4
  (PerPlayer 1)
  Equals
  [Moon, Triangle, Diamond]

instance HasAbilities DepthsOfDemheStepsOfThePalace where
  getAbilities (DepthsOfDemheStepsOfThePalace attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage DepthsOfDemheStepsOfThePalace where
  runMessage msg (DepthsOfDemheStepsOfThePalace attrs) =
    DepthsOfDemheStepsOfThePalace <$> runMessage msg attrs
