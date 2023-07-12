module Arkham.Act.Cards.TheBrokenRite
  ( TheBrokenRite(..)
  , theBrokenRite
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheBrokenRite = TheBrokenRite ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theBrokenRite :: ActCard TheBrokenRite
theBrokenRite = act (4, A) TheBrokenRite Cards.theBrokenRite Nothing

instance RunMessage TheBrokenRite where
  runMessage msg (TheBrokenRite attrs) = TheBrokenRite <$> runMessage msg attrs
