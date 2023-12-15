module Arkham.Act.Cards.TheKingsDecree (TheKingsDecree (..), theKingsDecree) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Prelude

newtype TheKingsDecree = TheKingsDecree ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theKingsDecree :: ActCard TheKingsDecree
theKingsDecree = act (2, A) TheKingsDecree Cards.theKingsDecree Nothing

instance RunMessage TheKingsDecree where
  runMessage msg (TheKingsDecree attrs) = TheKingsDecree <$> runMessage msg attrs
