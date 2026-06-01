module Arkham.Act.Cards.ShatteredMemories (shatteredMemories) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ShatteredMemories = ShatteredMemories ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shatteredMemories :: ActCard ShatteredMemories
shatteredMemories = act (1, A) ShatteredMemories Cards.shatteredMemories Nothing

instance RunMessage ShatteredMemories where
  runMessage msg (ShatteredMemories attrs) =
    runQueueT $ ShatteredMemories <$> liftRunMessage msg attrs
