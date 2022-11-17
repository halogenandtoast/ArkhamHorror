module Arkham.Act.Cards.JourneyToTheNexus
  ( JourneyToTheNexus(..)
  , journeyToTheNexus
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype JourneyToTheNexus = JourneyToTheNexus ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

journeyToTheNexus :: ActCard JourneyToTheNexus
journeyToTheNexus =
  act (1, A) JourneyToTheNexus Cards.journeyToTheNexus Nothing

instance RunMessage JourneyToTheNexus where
  runMessage msg (JourneyToTheNexus attrs) =
    JourneyToTheNexus <$> runMessage msg attrs
