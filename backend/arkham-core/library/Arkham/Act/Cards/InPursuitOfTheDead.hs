module Arkham.Act.Cards.InPursuitOfTheDead (
  InPursuitOfTheDead (..),
  inPursuitOfTheDead,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher

newtype InPursuitOfTheDead = InPursuitOfTheDead ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

inPursuitOfTheDead :: ActCard InPursuitOfTheDead
inPursuitOfTheDead =
  act (1, A) InPursuitOfTheDead Cards.inPursuitOfTheDead (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage InPursuitOfTheDead where
  runMessage msg (InPursuitOfTheDead attrs) = InPursuitOfTheDead <$> runMessage msg attrs
