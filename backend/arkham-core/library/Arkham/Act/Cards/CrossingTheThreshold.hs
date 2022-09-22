module Arkham.Act.Cards.CrossingTheThreshold
  ( CrossingTheThreshold(..)
  , crossingTheThreshold
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype CrossingTheThreshold = CrossingTheThreshold ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

crossingTheThreshold :: ActCard CrossingTheThreshold
crossingTheThreshold =
  act (1, A) CrossingTheThreshold Cards.crossingTheThreshold Nothing

instance RunMessage CrossingTheThreshold where
  runMessage msg (CrossingTheThreshold attrs) =
    CrossingTheThreshold <$> runMessage msg attrs
