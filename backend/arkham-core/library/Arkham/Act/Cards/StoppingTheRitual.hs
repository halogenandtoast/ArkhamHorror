module Arkham.Act.Cards.StoppingTheRitual
  ( StoppingTheRitual(..)
  , stoppingTheRitual
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype StoppingTheRitual = StoppingTheRitual ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

stoppingTheRitual :: ActCard StoppingTheRitual
stoppingTheRitual =
  act (3, A) StoppingTheRitual Cards.stoppingTheRitual Nothing

instance RunMessage StoppingTheRitual where
  runMessage msg (StoppingTheRitual attrs) =
    StoppingTheRitual <$> runMessage msg attrs
