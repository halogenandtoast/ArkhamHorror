module Arkham.Act.Cards.TheChamberOfStillRemains
  ( TheChamberOfStillRemains(..)
  , theChamberOfStillRemains
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher

newtype TheChamberOfStillRemains = TheChamberOfStillRemains ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theChamberOfStillRemains :: ActCard TheChamberOfStillRemains
theChamberOfStillRemains = act
  (2, A)
  TheChamberOfStillRemains
  Cards.theChamberOfStillRemains
  (Just $ GroupClueCost (PerPlayer 2) (LocationWithTitle "Chamber of Time"))

instance RunMessage TheChamberOfStillRemains where
  runMessage msg (TheChamberOfStillRemains attrs) =
    TheChamberOfStillRemains <$> runMessage msg attrs
