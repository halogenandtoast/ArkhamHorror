module Arkham.Act.Cards.TheIsleOfOriab (TheIsleOfOriab (..), theIsleOfOriab) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Prelude

newtype TheIsleOfOriab = TheIsleOfOriab ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theIsleOfOriab :: ActCard TheIsleOfOriab
theIsleOfOriab = act (2, A) TheIsleOfOriab Cards.theIsleOfOriab Nothing

instance RunMessage TheIsleOfOriab where
  runMessage msg (TheIsleOfOriab attrs) = TheIsleOfOriab <$> runMessage msg attrs
