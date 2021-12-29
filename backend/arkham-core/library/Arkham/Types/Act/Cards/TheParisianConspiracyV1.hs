module Arkham.Types.Act.Cards.TheParisianConspiracyV1
  ( TheParisianConspiracyV1(..)
  , theParisianConspiracyV1
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Timing

newtype TheParisianConspiracyV1 = TheParisianConspiracyV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theParisianConspiracyV1 :: ActCard TheParisianConspiracyV1
theParisianConspiracyV1 =
  act (1, A) TheParisianConspiracyV1 Cards.theParisianConspiracyV1 (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance HasAbilities TheParisianConspiracyV1 where
  getAbilities (TheParisianConspiracyV1 a) = [restrictedAbility a 1  (DoomCountIs $ AtLeast $ Static 3) $ Objective $ ForcedAbility $ RoundEnds When]

instance ActRunner env => RunMessage env TheParisianConspiracyV1 where
  runMessage msg (TheParisianConspiracyV1 attrs) =
    TheParisianConspiracyV1 <$> runMessage msg attrs
