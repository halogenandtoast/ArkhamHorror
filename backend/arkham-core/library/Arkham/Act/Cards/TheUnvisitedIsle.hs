module Arkham.Act.Cards.TheUnvisitedIsle (
  TheUnvisitedIsle (..),
  theUnvisitedIsle,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher

newtype TheUnvisitedIsle = TheUnvisitedIsle ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theUnvisitedIsle :: ActCard TheUnvisitedIsle
theUnvisitedIsle = act (1, A) TheUnvisitedIsle Cards.theUnvisitedIsle (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage TheUnvisitedIsle where
  runMessage msg (TheUnvisitedIsle attrs) = TheUnvisitedIsle <$> runMessage msg attrs
