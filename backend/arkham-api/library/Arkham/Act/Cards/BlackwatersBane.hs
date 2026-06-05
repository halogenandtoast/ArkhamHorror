module Arkham.Act.Cards.BlackwatersBane (blackwatersBane) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype BlackwatersBane = BlackwatersBane ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackwatersBane :: ActCard BlackwatersBane
blackwatersBane = act (3, A) BlackwatersBane Cards.blackwatersBane Nothing

instance RunMessage BlackwatersBane where
  runMessage msg (BlackwatersBane attrs) = BlackwatersBane <$> runMessage msg attrs
