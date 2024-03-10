module Arkham.Act.Cards.TheMoonsCore
  ( TheMoonsCore(..)
  , theMoonsCore
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheMoonsCore = TheMoonsCore ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theMoonsCore :: ActCard TheMoonsCore
theMoonsCore = act (3, A) TheMoonsCore Cards.theMoonsCore Nothing

instance RunMessage TheMoonsCore where
  runMessage msg (TheMoonsCore attrs) = TheMoonsCore <$> runMessage msg attrs
