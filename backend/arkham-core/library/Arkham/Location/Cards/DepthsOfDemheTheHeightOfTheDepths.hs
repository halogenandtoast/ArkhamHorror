module Arkham.Location.Cards.DepthsOfDemheTheHeightOfTheDepths
  ( depthsOfDemheTheHeightOfTheDepths
  , DepthsOfDemheTheHeightOfTheDepths(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype DepthsOfDemheTheHeightOfTheDepths = DepthsOfDemheTheHeightOfTheDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

depthsOfDemheTheHeightOfTheDepths
  :: LocationCard DepthsOfDemheTheHeightOfTheDepths
depthsOfDemheTheHeightOfTheDepths = location
  DepthsOfDemheTheHeightOfTheDepths
  Cards.depthsOfDemheTheHeightOfTheDepths
  4
  (PerPlayer 1)
  Equals
  [Moon, Triangle, Diamond]

instance HasAbilities DepthsOfDemheTheHeightOfTheDepths where
  getAbilities (DepthsOfDemheTheHeightOfTheDepths attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage DepthsOfDemheTheHeightOfTheDepths where
  runMessage msg (DepthsOfDemheTheHeightOfTheDepths attrs) =
    DepthsOfDemheTheHeightOfTheDepths <$> runMessage msg attrs
