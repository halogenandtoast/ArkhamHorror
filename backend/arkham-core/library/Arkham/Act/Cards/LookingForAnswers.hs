module Arkham.Act.Cards.LookingForAnswers
  ( LookingForAnswers(..)
  , lookingForAnswers
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype LookingForAnswers = LookingForAnswers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lookingForAnswers :: ActCard LookingForAnswers
lookingForAnswers = act (1, A) LookingForAnswers Cards.lookingForAnswers Nothing

instance RunMessage LookingForAnswers where
  runMessage msg (LookingForAnswers attrs) = LookingForAnswers <$> runMessage msg attrs
